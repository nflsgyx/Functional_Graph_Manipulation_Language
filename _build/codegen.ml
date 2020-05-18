open Ast
open Sast
open Lift
open EvalHelper

module StringMap = Map.Make(String)

let translate functions =

  let rec insert_value builder agg i v = L.build_insertvalue agg v i "tmp__" builder in
  let typ_of_lfexpr lfexpr = Func({
    return_typ = lfexpr.lreturn_typ;
    param_typs = List.map fst lfexpr.lparams;
  }) in

  let rec generate_seq n = if n >= 0 then (n :: (generate_seq (n-1))) else [] in

  let builtins = builtin_func_map in

  (* Build each function signature without building the body *)
  let function_decls : (L.llvalue * lfunc) StringMap.t =
    let function_decl m (name, lfexpr) =
      (* print_endline name; *)
      let ftype = ltype_of_lfexpr name lfexpr in
      StringMap.add name (L.define_function name ftype the_module, lfexpr) m
    in List.fold_left function_decl StringMap.empty functions
  in

  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  (*void foo(); ... void foo() {return;}*)
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in

  let concat_t : L.lltype =
    L.function_type str_t [| str_t; str_t |] in
  let concat_func : L.llvalue =
    L.declare_function "concat" concat_t the_module in

  (* Fill in the body for a given function *)
  let build_function_body (name, lfexpr) =
    let (the_function, _) = StringMap.find name function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and str_format_str = L.build_global_stringptr "%s\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in

    (* Unpacking args and env vars,
       skip for main as its env is empty and has no params *)
    let local_vars =
      let add_param m (t, n) p =
        let () = L.set_value_name n p in
        let local = L.build_malloc (ltype_of_typ t) n builder in
        let _ = L.build_store p local builder in
        StringMap.add n (t, local) m
      in
      let param_list = Array.to_list (L.params the_function) in
      let params =
        if List.length param_list <= 1 then StringMap.empty
        else List.fold_left2 add_param StringMap.empty lfexpr.lparams
          (List.tl param_list)
      in

      let env =
        if param_list = []
        then L.const_null void_ptr_t
        else List.hd param_list
      in

      let () = L.set_value_name "env" env in
      let env_void_ptr = L.build_malloc void_ptr_t "env" builder in
      let _ = L.build_store env env_void_ptr builder in
      let env_p = L.build_load env_void_ptr "env_p" builder in

      let params_of_lfexpr lfexpr = match lfexpr.lfvs with
          [] -> params
        | _ ->
          let ptr_of_fv (t, _) = L.pointer_type (ltype_of_typ t) in
          let env_struct = L.struct_type context (Array.of_list
            (List.map ptr_of_fv lfexpr.lfvs)) in
          let env_ptr_t = L.pointer_type env_struct in
          let env_ptr = L.build_bitcast env_p env_ptr_t "env_p" builder in
          let env_val = L.build_load env_ptr "env_val" builder in
          let add_free_var m (t, n) idx =
            let free_var = L.build_extractvalue env_val idx "tmp_" builder in
            StringMap.add n (t, free_var) m
          in
          let fvs_count = List.length lfexpr.lfvs in
          List.fold_left2 add_free_var params lfexpr.lfvs
            (List.rev (generate_seq (fvs_count - 1)))
      in

      let params_fvs = match name with
          "main" -> params
        | _ -> params_of_lfexpr lfexpr
      in

      (* Allocate a closure of the function within itself for recursive calls *)
      let clsr_t = ltype_of_clsr name lfexpr in
      let clsr_p = L.build_malloc clsr_t lfexpr.lname builder in
      let clsr_val = List.fold_left2 (insert_value builder)
        (L.const_null clsr_t) [0;1] [the_function;env_p] in
      let _ = L.build_store clsr_val clsr_p builder in
      let func_t  = typ_of_lfexpr lfexpr in
      StringMap.add lfexpr.lname (func_t, clsr_p) params_fvs
    in

    let rec expr builder (m : (typ * L.llvalue) StringMap.t) ((ty, e) : sexpr) =

      let lookup n =
        let (_, llval) = try StringMap.find n m with
            Not_found ->
            if StringMap.mem n builtins then StringMap.find name builtins
            else raise (Failure ("Codegen Variable not found: " ^ n))
        in llval
      in

      let lookup_lval n =
        match n with
          SLId n -> lookup n
        | SLArrEl SOneDim(n, idx) ->
          L.build_in_bounds_gep (lookup n) [|(L.const_int i32_t 0);(expr builder m idx)|] (n ^ "el") builder;
        | _ -> raise (Failure "not multidim array")
      and
        load_val n =
        match n with
        | SOneDim(n, idx) ->
          let name = (n ^ "el") in
          let p = L.build_gep (lookup n) [|(L.const_int i32_t 0);(expr builder m idx)|] name builder in
          L.build_load p name builder
        | _ -> raise (Failure "not multidim array")
      in

      let build_clsr clsr =
        let fvs = List.map snd clsr.free_vars in
        let llfvs = List.map lookup fvs in
        let fvs_t = List.map ltype_of_typ (List.map fst clsr.free_vars) in
        let fvs_ptr_t = List.map L.pointer_type fvs_t in
        let env_struct_t = L.struct_type context (Array.of_list fvs_ptr_t) in
        let env_struct = L.build_malloc env_struct_t "tmp_" builder in
        let idxs = List.rev (generate_seq ((List.length fvs) - 1)) in
        let env_val = List.fold_left2 (insert_value builder)
            (L.const_null env_struct_t) idxs llfvs in
        let _ = L.build_store env_val env_struct builder in
        let env_struct_p = L.build_bitcast env_struct void_ptr_t "env_p" builder in

        (* Pack the function ptr and the env ptr into the closure struct *)
        let func_name = "f" ^ (string_of_int clsr.ind) in
        let (llfunc, sfexpr) = StringMap.find func_name function_decls in
        let llclosure_struct_t = ltype_of_clsr func_name sfexpr in
        let clsr_val = List.fold_left2
            (insert_value builder)
            (L.const_null llclosure_struct_t)
            [0;1]
            [llfunc; env_struct_p]
        in clsr_val
      in

      match e with
        SStrLit s -> L.build_global_stringptr s "str" builder
      | SLiteral x -> L.const_int i32_t x
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SFliteral x -> L.const_float_of_string float_t x
      | SId s -> L.build_load (lookup s) s builder
      | SNoexpr -> L.const_int i32_t 0

      | SArrEl s -> load_val s
      | SSeq s -> (match s with
          [] -> raise (Failure "empty sequence not allowed")
        | _ ->
          (* let _ = Printf.printf("hi there") in *)
          let list_push_right_formals =
            let fdecl = StringMap.find "list_push_left" Semant.builtinMap in
            let fdecl = match fdecl with
              | Func(x) -> x
              | _ -> raise(Failure "never here")
            in
            fdecl.param_typs
          in
          let tmp = build_lst_init builder in
          let rec push_all_to_list (lst: L.llvalue) (seq : sexpr list) : L.llvalue = match seq with
            hd::tl ->
            let arg_llvals = List.map2 (ll_get_void_addr builder) [tmp; (expr builder m hd)] list_push_right_formals in
            let lst = L.build_call list_push_right_func_f (Array.of_list arg_llvals) "list_pupulate" builder in
            push_all_to_list lst tl
          | [] -> tmp
          in
          push_all_to_list tmp s
      )

      | SNodeExpr (idx, v) ->
      let assign_formals =
        let fdecl = StringMap.find "assign_node" Semant.builtinMap in
        let fdecl = match fdecl with
            | Func(x) -> x
            | _ -> raise(Failure "never here")
        in
        fdecl.param_typs
      in
      (match assign_formals with a1::_::a3::[] ->
      let tmp = build_node_init builder in
      let arg_llvals = [|ll_get_void_addr builder tmp a1; expr builder m idx; ll_get_void_addr builder (expr builder m v) a3|] in
      let _ = L.build_call assign_node_func_f arg_llvals "" builder in
      tmp
      | _ -> raise (Failure "never here"))

      | SEdgeExpr (ndf, w, ndt)  ->
        let assgin_formals =
          let fdecl = StringMap.find "assign_edge" Semant.builtinMap in
          let fdecl = match fdecl with
            | Func(x) -> x
            | _ -> raise(Failure "never here")
          in
          fdecl.param_typs
        in
          (match assgin_formals with a1::a2::a3::a4::[] ->
          let tmp = build_edge_init builder in
            let arg_llvals = [|
            ll_get_void_addr builder tmp a1;
            ll_get_void_addr builder (expr builder m ndf) a2;
            ll_get_void_addr builder (expr builder m ndt) a3;
            ll_get_void_addr builder (expr builder m w) a4
            |] in
            let _ = L.build_call assign_edge_func_f arg_llvals "" builder in
          tmp
          | _ -> raise (Failure "never here"))
      | SAssign (s, e) ->
        let e' = expr builder m e in
        ignore(L.build_store e' (lookup_lval s) builder); e'
       | SBinop (((A.String,_ ) as e1), op, ((A.String,_ ) as e2)) ->
        let e1' = expr builder m e1
        and e2' = expr builder m e2 in
        (match op with
           A.Concat  ->  L.build_call concat_func [| e1' ; e2' |] "concat" builder
         | _ ->
           raise (Failure "internal error: semant should have rejected and/or on string")
        )

      | SBinop (e1, op, e2) ->
        let (t, _) = e1
        and e1' = expr builder m e1
        and e2' = expr builder m e2 in
        (match snd e1, snd e2 with
           _ -> (match t with
               Float -> (match op with
                   Add     -> L.build_fadd
                 | Sub     -> L.build_fsub
                 | Mult    -> L.build_fmul
                 | Div     -> L.build_fdiv
                 | Equal   -> L.build_fcmp L.Fcmp.Oeq
                 | Neq     -> L.build_fcmp L.Fcmp.One
                 | Less    -> L.build_fcmp L.Fcmp.Olt
                 | Leq     -> L.build_fcmp L.Fcmp.Ole
                 | Greater -> L.build_fcmp L.Fcmp.Ogt
                 | Geq     -> L.build_fcmp L.Fcmp.Oge
                 | _ ->
                   raise (Failure ("internal error: "
                     ^ "semant should have rejected and/or/concat on float"))
               ) e1' e2' "tmp" builder
             | Int -> (match op with
                 | Add     -> L.build_add
                 | Sub     -> L.build_sub
                 | Mult    -> L.build_mul
                 | Div     -> L.build_sdiv
                 | And     -> L.build_and
                 | Or      -> L.build_or
                 | Equal   -> L.build_icmp L.Icmp.Eq
                 | Neq     -> L.build_icmp L.Icmp.Ne
                 | Less    -> L.build_icmp L.Icmp.Slt
                 | Leq     -> L.build_icmp L.Icmp.Sle
                 | Greater -> L.build_icmp L.Icmp.Sgt
                 | Geq     -> L.build_icmp L.Icmp.Sge
                 | Concat -> raise (Failure "internal error: semant should have rejected concat on int")
               ) e1' e2' "tmp" builder
             | Bool -> (match op with
                   And     -> L.build_and
                 | Or      -> L.build_or
                 | Equal   -> L.build_icmp L.Icmp.Eq
                 | Neq     -> L.build_icmp L.Icmp.Ne
                 | Less    -> L.build_icmp L.Icmp.Slt
                 | Leq     -> L.build_icmp L.Icmp.Sle
                 | Greater -> L.build_icmp L.Icmp.Sgt
                 | Geq     -> L.build_icmp L.Icmp.Sge
                 | _         -> raise (Failure ("operation " ^ (string_of_op op)
                                                ^ " not implemented for type "
                                                ^ (string_of_typ t)))
               ) e1' e2' "tmp" builder
             | String -> (match op with
                A.Concat  ->  L.build_call concat_func [| e1' ; e2' |] "concat" builder
                | _ ->  raise (Failure "internal error: semant should have rejected and/or on string"))
             | _ -> (match op with
                   Equal -> (L.build_icmp L.Icmp.Eq) e1' e2' "tmp" builder
                 | Neq -> (L.build_icmp L.Icmp.Ne) e1' e2' "tmp" builder
                 | _ -> raise (Failure ("operation " ^ (string_of_op op)
                                        ^ " not implemented for type "
                                        ^ (string_of_typ t))))
           ))
      | SUnop(op, e) ->
        let (t, _) = e in
        let e' = expr builder m e in
        (match op with
           Neg when t = Float -> L.build_fneg
         | Neg when t = Int -> L.build_neg
         | Not when t = Bool -> L.build_not
         | _ -> raise (Failure ("operation " ^ (string_of_uop op) ^
                                " not implemented for type "
                                ^ (string_of_typ t)))) e' "tmp" builder
      | SClosure clsr -> build_clsr clsr
      | SCall ("print", [e]) | SCall ("printb", [e]) ->
        L.build_call printf_func [| int_format_str ; (expr builder m e) |]
          "printf" builder
      | SCall ("printf", [e] ) ->
        L.build_call printf_func [| float_format_str ; (expr builder m e) |]
          "printf" builder
      | SCall ("prints", [e] ) ->
        L.build_call printf_func [| str_format_str ; (expr builder m e) |]
          "printf" builder
      | SCall (sname, exprs) when StringMap.mem sname EvalHelper.builtin_func_map ->
        let safe_map_find key map =
          let (_, func) = StringMap.find key map in func
        in
        let func = safe_map_find sname EvalHelper.builtin_func_map in
        let arg_llvals = List.rev (List.map (expr builder m) exprs) in
        let fdecl = StringMap.find sname Semant.builtinMap in
        let fdecl = match fdecl with
            | Func(x) -> x
            | _ -> raise(Failure "never here")
        in
        let ftyp = fdecl.return_typ and formals_typs = fdecl.param_typs in
        let wildcard_types = get_wildcard_types formals_typs (List.rev exprs) in
        let arg_llvals = List.map2 (ll_get_void_addr builder) arg_llvals formals_typs in
        let arg_llvals = Array.of_list arg_llvals in
        let result = (match fdecl.return_typ with
              A.Void -> ""
            | _ -> sname ^ "_result") in
        let ret_ptr = L.build_call func arg_llvals result builder in
        (match ftyp with
          A.WildCard(wid) -> ll_voidptr2wildcard builder ret_ptr wildcard_types wid
        | _ -> ret_ptr
        )
      | SCall(sname, args) ->
        let clsr_val = expr builder m (ty, SId(sname)) in
        let func_ptr = L.build_extractvalue clsr_val 0 "fp" builder in
        let env_ptr = L.build_extractvalue clsr_val 1 "envp" builder in
        let llargs = env_ptr :: (List.map (expr builder m)
          (List.rev args)) in
        let result =
            (match ty with Void -> "" | _ -> "_result") in
        L.build_call func_ptr (Array.of_list llargs) result builder
      | _ -> raise (Failure "not implemented in codegen")
    in
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        None -> ignore (instr builder)
      | Some _ -> ()
    in

    let rec stmt builder m = function
        SExpr e -> let _ = expr builder m e in (builder, m)
      | SBlock(sl) -> let helper (bldr, map) = stmt bldr map in
                      let (b, _) = List.fold_left helper (builder, m) sl in
                      (b, m)
    (*   | SStructDef(n, mem) ->
        let struct_t = SStruct({
            sstruct_name = n;
            sincomplete = false;
            signore = false;
            smembers = List.fold_left
              (fun m (t, n, opt_se) -> StringMap.add n (t, opt_se) m)
              StringMap.empty mem;
          }) in
        (builder, StringMap.add n (struct_t, L.const_int i1_t 0) m) *)
      | SVDecl(t, n, se) ->
        let alloc_clsr clsr =
          let func_name = ("f" ^ (string_of_int clsr.ind)) in
          let (_, lfexpr) = StringMap.find func_name function_decls in
          let func_t = L.pointer_type (ltype_of_lfexpr func_name lfexpr) in
          let llclosure_struct_t = L.struct_type context [|func_t; void_ptr_t|] in
          L.build_malloc llclosure_struct_t n builder
        in
        let se = match t with
            Graph(_) when se == None -> Some((Graph(WildCard(1), WildCard(2)),SCall("init_graph", [])))
(*           | Node(_) when se == None -> Some((Node(WildCard(1)),SCall("init_node", [])))*)
          | _ -> se
        in
(*         let _ = match r with
          Graph(_) -> stmt builder m SExpr() *)
        let (builder, local_var) = match se with
            None ->
            (* print_endline  ("none" ^ n); *)
            let local_var = L.build_malloc (ltype_of_typ t) n builder in
            (builder, local_var)
          | Some(e) ->
            (* print_endline  ("some" ^n); *)
            let (_, ex) = e in
            let local_var = match ex with
                SClosure(clsr) -> alloc_clsr clsr
              | _ -> L.build_malloc (ltype_of_typ t) n builder
            in
            let e' = expr builder m e in
            let _ = L.build_store e' local_var builder in
            (builder, local_var)
        in
        let m' = StringMap.add n (t, local_var) m in
        (builder, m')
      | SIf (pred, then_stmts, else_stmts) ->
        let bool_val = expr builder m pred in
        let merge_bb = L.append_block context "merge" the_function in
        let branch_instr = L.build_br merge_bb in
        let then_bb = L.append_block context "then" the_function in
        let (then_builder, _) =
          stmt (L.builder_at_end context then_bb) m then_stmts in
        let () = add_terminal then_builder branch_instr in
        let else_bb = L.append_block context "else" the_function in
        let (else_builder, _) =
          stmt (L.builder_at_end context else_bb) m else_stmts in
        let () = add_terminal else_builder branch_instr in
        let _ = L.build_cond_br bool_val then_bb else_bb builder in
        (L.builder_at_end context merge_bb, m)

      | SReturn e ->
        let _ = match lfexpr.lreturn_typ with
            Void -> L.build_ret_void builder
          | _ -> L.build_ret (expr builder m e) builder
        in (builder, m)


       | SWhile (predicate, body) ->
        let pred_bb = L.append_block context "while" the_function in
        ignore(L.build_br pred_bb builder);

        let body_bb = L.append_block context "while_body" the_function in
        let (b1,_) = stmt (L.builder_at_end context body_bb) m body in
        add_terminal b1 (L.build_br pred_bb);

        let pred_builder = L.builder_at_end context pred_bb in
        let bool_val = expr pred_builder m predicate in

        let merge_bb = L.append_block context "merge" the_function in
        ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
        (L.builder_at_end context merge_bb, m)

      (* Implement for loops as while loops *)
      | SFor (e1, e2, e3, body) -> stmt builder m ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )

    in

    let (builder, _) = stmt builder local_vars lfexpr.lbody in

    (* add a return if the last block falls off the end *)
    add_terminal builder (match lfexpr.lreturn_typ with
          Void -> L.build_ret_void
        | String -> L.build_ret (L.build_global_stringptr "" "str" builder)
        | Float -> L.build_ret (L.const_float_of_string float_t "0.0")
        | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))

  in
  List.iter build_function_body functions;
  the_module
