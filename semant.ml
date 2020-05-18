(* Authors: Sam Jayasinghe, Claire Adams, Crystal Ren, Cindy Le *)

open Ast
open Sast

module StringMap = Map.Make (String)

exception Type_mismatch of string
exception Undeclared_reference of string

let prefix (typ: Ast.typ) : string = match typ with
  Lst _ -> "list"
| Node _ -> "node"
| Edge _ -> "edge"
| Graph _ -> "graph"
| _ -> raise (Failure "prefix Not Implemented")

let rec replace_wildcards typ (wildcard_types : (int, Ast.typ)  Hashtbl.t) =
  match typ with
  | WildCard(wid) ->
    Hashtbl.find wildcard_types wid
  | Lst(typ) -> Lst(replace_wildcards typ wildcard_types)
  | Node(typ) -> Node(replace_wildcards typ wildcard_types)
  | Edge(typ) -> Edge(replace_wildcards typ wildcard_types)
  | Graph(typ1, typ2) ->
    let typ1 = replace_wildcards typ1 wildcard_types
    and typ2 = replace_wildcards typ2 wildcard_types
    in Graph(typ1, typ2)
  | typ -> typ

let type2enum (typ: Ast.typ) : Ast.expr =
  match typ with
  | Int -> Literal(1)
  | _ -> raise (Failure "Not Implemented type 2 enum")

let rec get_wildcard_enum (typ: Ast.typ): Ast.expr list =
  match typ with
  | Lst(wtyp) -> [type2enum wtyp]
  | Edge(wtyp) -> [type2enum wtyp]
  | Node(wtyp) -> [type2enum wtyp]
  | Graph(wtyp1, wtyp2) -> [type2enum wtyp1; type2enum wtyp2]
  | _ -> raise (Failure "shouldn't reach here")

let builtinsFunc = [
  ("print", Func({ param_typs = [Int]; return_typ = Void }));
  ("printb", Func({ param_typs = [Bool]; return_typ = Void }));
  ("printf", Func({ param_typs = [Float]; return_typ = Void }));
  ("prints", Func({ param_typs = [String]; return_typ = Void }));
  ("list_push_left", Func({param_typs = [Lst(WildCard(1)); WildCard(1)]; return_typ = Lst(WildCard(1)) }));
  ("list_push_right", Func({param_typs = [Lst(WildCard(1)); WildCard(1)]; return_typ = Lst(WildCard(1)) }));
  ("list_pop_left", Func({param_typs = [Lst(WildCard(1))]; return_typ = WildCard(1) }));
  ("list_pop_right",  Func({param_typs = [Lst(WildCard(1))]; return_typ = WildCard(1) }));
  ("list_peek_left",  Func({param_typs = [Lst(WildCard(1))]; return_typ = WildCard(1) }));
  ("list_peek_right",  Func({param_typs = [Lst(WildCard(1))]; return_typ = WildCard(1) }));
  ("list_copy",  Func({param_typs = [Lst(WildCard(1))]; return_typ = Lst(WildCard(1)) }));
  ("list_length",  Func({param_typs = [Lst(WildCard(1))]; return_typ = Int }));
  ("list_remove_by_index",  Func({param_typs = [Lst(WildCard(1)); Int]; return_typ = WildCard(1) }));
  ("node_get_index",  Func({param_typs = [Node(WildCard(1))]; return_typ = Int }));
  ("node_get_value",  Func({param_typs = [Node(WildCard(1))]; return_typ = WildCard(1) }));
  ("node_get_edges",  Func({param_typs = [Node(WildCard(1))]; return_typ = Lst(Edge(WildCard(1))) }));
  ("node_set_index", Func({param_typs = [Node(WildCard(1)); Int]; return_typ = Void }));
  ("node_set_value",  Func({param_typs = [Node(WildCard(1)); WildCard(1)]; return_typ = Void }));
  ("assign_node",  Func({param_typs = [Node(WildCard(1)); Int; WildCard(1)]; return_typ = Void }));
  ("node_to_s", Func({param_typs = [Node(WildCard(1)); Int]; return_typ = String }));
  ("edge_get_from", Func({param_typs = [Edge(WildCard(1))]; return_typ = Node(WildCard(1)) }));
  ("edge_get_to", Func({param_typs = [Edge(WildCard(1))]; return_typ = Node(WildCard(1)) }));
  ("edge_get_weight", Func({param_typs = [Edge(WildCard(1))]; return_typ = WildCard(1) }));
  ("edge_set_from", Func({param_typs = [Edge(WildCard(1)); Node(WildCard(1))]; return_typ = Void }));
  ("edge_set_to", Func({param_typs = [Edge(WildCard(1)); Node(WildCard(1))]; return_typ = Void }));
  ("edge_set_weight", Func({param_typs = [Edge(WildCard(1)); WildCard(1)]; return_typ = Void }));
  ("edge_to_s", Func({param_typs = [Edge(WildCard(1)); Int]; return_typ = String }));
  ("assign_edge", Func({param_typs = [Edge(WildCard(1)); Node(WildCard(1)); Node(WildCard(1)); WildCard(1)]; return_typ = Void }));
  ("graph_add_node", Func({param_typs = [Graph(WildCard(1), WildCard(2)); Node(WildCard(1))]; return_typ = Void }));
  ("graph_add_edge", Func({param_typs = [Graph(WildCard(1), WildCard(2)); Edge(WildCard(1))]; return_typ = Int }));
  ("graph_add_edge_by_node", Func({param_typs = [Graph(WildCard(1), WildCard(2)); Node(WildCard(1)); Node(WildCard(1)); WildCard(1)]; return_typ = Int }));
  ("graph_get_node", Func({param_typs = [Graph(WildCard(1), WildCard(2)); Int]; return_typ = Node(WildCard(1)) }));
  ("graph_add_edge_by_id", Func({param_typs = [Graph(WildCard(1), WildCard(2)); Int; Int; WildCard(1)]; return_typ = Int }));
  ("graph_remove_node", Func({param_typs = [Graph(WildCard(1), WildCard(2)); Node(WildCard(1))]; return_typ = Void }));
  ( "graph_remove_edge", Func({param_typs = [Graph(WildCard(1), WildCard(2)); Edge(WildCard(1))]; return_typ = Void }));
  ( "graph_copy", Func({param_typs = [Graph(WildCard(1), WildCard(2))]; return_typ = Graph(WildCard(1), WildCard(2)) }));
  ( "graph_set_flags", Func({param_typs = [Graph(WildCard(1), WildCard(2)); Int]; return_typ = Void }));
  ( "node_set_flag", Func({param_typs = [Node(WildCard(1)); Int]; return_typ = Void }));
  ("node_get_flag", Func({param_typs = [Node(WildCard(1))]; return_typ = Int }));
  ("graph_to_s", Func({param_typs = [Graph(WildCard(1), WildCard(2)); Int; Int]; return_typ = String }));
  ("init_graph", Func({param_typs = []; return_typ = Graph(WildCard(1), WildCard(2))}));
  ("init_node", Func({param_typs = []; return_typ = Node(WildCard(1)) }));
]



let is_builtin fname =
  let looking_for func = (fst func = fname) in
  List.exists looking_for builtinsFunc


let makeMapFromBuiltinsFunc map arrElem =
  StringMap.add (fst arrElem) (snd arrElem) map

let builtinMap = List.fold_left
  makeMapFromBuiltinsFunc StringMap.empty builtinsFunc

let rec compare_typs t1 t2 = match t1, t2 with
(*   | SArray(SAny), SArray(_) -> true
  | SArray(_), SArray(SAny) -> true *)
  | Func(f1), Func(f2) ->
    let same_ret = compare_typs f1.return_typ f2.return_typ in
    let same_args = List.for_all2 compare_typs f1.param_typs f2.param_typs
    in same_ret && same_args
  | _ -> t1 = t2

let check_asn lvalue_t rvalue_t =
  let found_match = compare_typs lvalue_t rvalue_t in
  if found_match
  then lvalue_t
  else
    (print_endline(Printexc.raw_backtrace_to_string(Printexc.get_callstack 100));
     raise (Type_mismatch ("type mismatch error " ^
        string_of_typ lvalue_t ^ " " ^ string_of_typ rvalue_t)))



(* This function takes a tuple with the type and the map
 * as well as the variable name and the context map.
 * The map in the tuple is used for the member fields
 * in structs. The map is None unless you are adding a new
 * struct type. *)
let add_to_ctxt (v_type : typ) (v_name : string)
    (ctxt : typ StringMap.t list) =
  let map = List.hd ctxt in
  try
    match (StringMap.find v_name map) with
      _ -> raise (Failure (v_name ^ " already declared"))
  with Not_found ->
    let newMap = StringMap.add v_name v_type map in
    newMap::List.tl ctxt

(* Returns a tuple with the type and the map and if
 * the variable is initalized or not. The type and
 * map are optional. *)
let rec find_in_ctxt (v_name : string) (ctxt : typ StringMap.t list) =
  try
    StringMap.find v_name (List.hd ctxt)
  with Not_found -> match List.tl ctxt with
      [] -> raise (Undeclared_reference ("undeclared reference " ^ v_name))
    | tail -> find_in_ctxt v_name tail

let get_arr_el_ty = function
    Array(ty, _) -> ty
  | _ -> raise (Failure "error wrong array type")

let rec type_of_arrEl a ctxt = match a with
  OneDim(name, _) -> get_arr_el_ty (find_in_ctxt name ctxt)
| MultiDim(var, _) -> get_arr_el_ty (type_of_arrEl var ctxt)


(*

let rec ignore_structs t = match t with
    SStruct(st) ->
    SStruct({
        sstruct_name = st.sstruct_name;
        smembers = st.smembers;
        sincomplete = st.sincomplete;
        signore = true;
      })
  | SArray(t) -> SArray(ignore_structs t)
  | SFunc(f) -> SFunc({
      sparam_typs = List.map ignore_structs f.sparam_typs;
      sreturn_typ = ignore_structs f.sreturn_typ;
      sbuiltin = f.sbuiltin;
    })
| _ -> t
 *)

and check_lval (ctxt : typ StringMap.t list) = function
    LId(l)     -> SLId(l)
  | LArrEl(ae) -> SLArrEl(check_arrEl ctxt ae)

and check_arrEl (ctxt : typ StringMap.t list) = function
    OneDim(name, e) -> SOneDim(name, check_expr ctxt e)
  | MultiDim(var, e) -> SMultiDim(check_arrEl ctxt var, check_expr ctxt e)

and check_seq (ctxt : typ StringMap.t list) (s : expr list) : (sexpr list) =
  match s with
    [] -> []
  | x::y::rest ->
    let (t1, _) = check_expr ctxt x
    and t2, _ = check_expr ctxt y in
    if t1 = t2 then (check_seq ctxt [x]) @ check_seq ctxt (y::rest)
      else raise (Failure("array element not the same type"))
  | [x] -> [check_expr ctxt x]

and type_of_seq (ctxt : typ StringMap.t list) (s : expr list ) : typ =
  match s with
    [] -> Empty
  | hd::_ -> Lst (fst (check_expr ctxt hd))

(* Returns a tuple with a map and another tuple.
 * The second tuple has the type and the stype. *)
and check_expr (ctxt : typ StringMap.t list) = function
  | Literal(x) -> (Int, SLiteral x)
  | BoolLit(x) -> (Bool, SBoolLit x)
  | Fliteral(x) -> (Float, SFliteral x)
  | StrLit(x) -> (String, SStrLit x)

  | Binop(e1, op, e2) as e->
    let (t1, e1') = check_expr ctxt e1 in
    let (t2, e2') = check_expr ctxt e2 in
    let same = (t1 = t2) in
        (* Determine expression type based on operator and operand types *)
        let ty = match op with
            Add | Sub | Mult | Div when same && t1 = Int   -> Int
          | Add | Sub | Mult | Div when same && t1 = Float -> Float
          | Concat when same && t1 = String -> String
          | Equal | Neq            when same               -> Bool
          | Less | Leq | Greater | Geq
            when same && (t1 = Int || t1 = Float) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
              Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
    in (ty, SBinop((t1, e1'), op, (t2, e2')))
  | Unop(op, e) as ex->
    let (t, e') = check_expr ctxt e in
    let ty = match op with
        Neg when t = Int || t = Float -> t
      | Not when t = Bool -> Bool
      | _ -> raise (Failure ("illegal unary operator " ^
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
    in (ty, SUnop(op, (t, e')))

  | ArrEl v -> (type_of_arrEl v ctxt, SArrEl(check_arrEl ctxt v))
  | Seq s -> (type_of_seq ctxt s, SSeq(check_seq ctxt s))
  | NodeExpr(idx, v) ->
    let idx_typ, idx_v = check_expr ctxt idx in
    let v_typ, v_v =  check_expr ctxt v in
    if idx_typ != Int then raise (Failure "index not int") else
    (Node(v_typ), SNodeExpr((idx_typ, idx_v), (v_typ, v_v)))
  | EdgeExpr(ndf, w, ndt) ->
    let ndf_typ, ndf_v =  check_expr ctxt ndf in
    let w_typ, w_v =  check_expr ctxt w in
    let ndt_typ, ndt_v =  check_expr ctxt ndt in
    if ndf_typ = ndt_typ then (Edge(fst (check_expr ctxt w)), SEdgeExpr( (ndf_typ, ndf_v), (w_typ, w_v), (ndt_typ, ndt_v) ) )
    else raise (Failure "edge's end nodes have different type")

  | Assign(var, e) as ex ->
    let lt = match var with
        LId var -> find_in_ctxt var ctxt
      | LArrEl v -> type_of_arrEl v ctxt
    and (rt, e') = check_expr ctxt e in
    (check_asn lt rt, SAssign(check_lval ctxt var, (rt, e')))

  | Id(n) ->
    let t = find_in_ctxt (if String.contains n '~' then String.sub n 1
        ((String.length n) - 1) else n) ctxt in
    if (String.contains n '~') then
        (t, SId (String.sub n 1 ((String.length n) - 1)))
    else (match t with
      Func(f) when is_builtin n ->
        let ft = match StringMap.find n builtinMap with
            Func(func) -> func | _ -> raise (Failure ("shouldn't happen")) in
        check_expr ctxt (FExpr({
          name = "";
          typ = ft.return_typ;
          params = List.mapi (fun i t -> (t, "__p" ^ (string_of_int i))) ft.param_typs;
          body = if ft.return_typ == Void
            then Expr(Call( ("~" ^ n) , List.mapi (fun i _ -> Id("__p" ^ (string_of_int i)) ) ft.param_typs))
            else Block([
              VDecl(ft.return_typ, "__ret", Some(Call( ("~" ^ n),
                List.mapi (fun i _ -> Id("__p" ^ (string_of_int i)) ) ft.param_typs)));
              Return(Id("__ret"));
            ]);
        }))
      | _ -> (t, SId n))

  | Call(fname, args) as call ->
    let t = find_in_ctxt fname ctxt in
    let fd = (match t with
      | Func(func_typ) -> func_typ) in
    let param_length = List.length fd.param_typs in
    if List.length args != param_length then
      raise (Failure ("expecting " ^ string_of_int param_length ^
                      " arguments in " ^ string_of_expr call))
    else let check_call param_typs ((se : sexpr), expr)  (wildcard_types : (int, typ) Hashtbl.t) =
          let ft = replace_wildcards param_typs wildcard_types in
          let (et, e') = se in
          let err = "illegal argument found " ^ string_of_typ et ^
                     " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr expr
          in (check_asn ft et, e')
      in
      let get_wildcard_types p_typs (args: sexpr list) =
        let update_wildcard_types (result: (int, Ast.typ) Hashtbl.t) (a: typ) b =
          let et, _ = b in
          let rec search_wildcard_type typ1 typ2 = (match typ1, typ2 with
          | WildCard(wid), typ -> Hashtbl.replace result wid typ
          | Lst(typ1), Lst(typ2) -> search_wildcard_type typ1 typ2
          | Node(typ1), Node(typ2) -> search_wildcard_type typ1 typ2
          | Edge(typ1), Edge(typ2) -> search_wildcard_type typ1 typ2
          | Graph(ftyp1, ftyp2), Graph(typ1, typ2) ->
            let _ = search_wildcard_type ftyp1 typ1 in
            search_wildcard_type ftyp2 typ2
          | _ -> ())
          in
          let _ = search_wildcard_type a et
          in result
        in
        List.fold_left2 update_wildcard_types (Hashtbl.create 42) p_typs args
      in
      (* wildcard_types = [Int] *)
      let helper_expr e = check_expr ctxt e in
      let sargs = List.map helper_expr args in
      let wildcard_types = get_wildcard_types fd.param_typs sargs in
      let check_call2 param_typs se_e = check_call param_typs se_e wildcard_types in
      let merge a b = a, b in
      let se_e = List.map2 merge sargs args in
      let args' = List.map2 check_call2 fd.param_typs se_e in
      let typ = replace_wildcards fd.return_typ wildcard_types in
      (typ, SCall(fname, args'))

  | CMCall("to_s", hd::args) ->
        let typ, _ = check_expr ctxt hd in
        let fname = (prefix typ) ^ "_" ^ "to_s" in
        let typ_args = get_wildcard_enum typ in
        check_expr ctxt (Call (fname, hd::typ_args))
        (* TODO only support int here *)
  | CMCall(fname, hd::args) ->
        let typ, _ = check_expr ctxt hd in
        let fname = (prefix typ) ^ "_" ^ fname in
        check_expr ctxt (Call (fname, hd::args))
  | CMCall(_, _) -> raise (Failure "Grammar error")

  | FExpr(fexpr) ->
    let func_t = Func({
      return_typ = fexpr.typ;
      param_typs = List.map (fun x -> fst x) fexpr.params;
    }) in
    let create_scope list =
      let rec helper m = function
          [] -> m
        | (t, n)::tl ->
          let new_m = StringMap.add n t m in
          helper new_m tl
      in
      if fexpr.name <> ""
        then helper (StringMap.add fexpr.name func_t StringMap.empty) list
        else helper StringMap.empty list
    in
    let func_scope = create_scope fexpr.params in
    let (_, return_t, sl) = check_stmt (func_scope::ctxt) fexpr.body in
    ignore (check_asn return_t fexpr.typ);
    (func_t, SFExpr({
         sname = fexpr.name;
         styp = fexpr.typ;
         sparams = fexpr.params;
         sbody = sl;
       }))

  | Noexpr -> (Void, SNoexpr)
  | _ as x -> print_endline(Ast.string_of_expr x);
    raise (Failure "not implemented in semant")



and check_stmt_list (ctxt : typ StringMap.t list) = function
    [] -> (ctxt, Void, [])
  | hd::tl ->
    let (nctxt, t, ss) = check_stmt ctxt hd in
    let (nctxt, t_rest, ssl) = check_stmt_list nctxt tl in
    let ret =
      if t = Void
      then t_rest
      else (if List.length tl <> 0 then raise
        (Failure "dead code after return") else (); t)
    in
    (nctxt, ret, ss::ssl) (* returned something *)

and check_bool_expr (ctxt : typ StringMap.t list) e =
  let (t, st) = check_expr ctxt e in
  if (t <> Bool) then
    raise (Failure("Error: " ^ string_of_typ t ^ " is not a boolean type"))
  else (t, st)

(* returns the map, type, stype *)
and check_stmt (ctxt : typ StringMap.t list) = function
  | VDecl(t, n, v) ->
  (match v with
     None -> (add_to_ctxt t n ctxt, Void, SVDecl(t, n, None))
   | Some(e) ->
     let (t_v, s_v) = check_expr ctxt e in
     let nctxt = add_to_ctxt t n ctxt in
     (nctxt, Void, SVDecl((check_asn t_v t), n, Some((t_v, s_v)))))
  | Block(sl) ->
    let (_, ret_t, sl') = check_stmt_list ctxt sl in
    (ctxt, ret_t, SBlock(sl'))
  | Expr(e) ->
    let (t, ss) = check_expr ctxt e in (ctxt, Void, SExpr((t, ss)))

  | Return(e) ->
    let (t, ss) = check_expr ctxt e in
    (ctxt, t, SReturn((t, ss)))
  | For (s1, e2, e3, st) ->
    let (ctxt1, s1') =
      (let (t_i, si) = check_expr (StringMap.empty::ctxt) s1 in (ctxt, (t_i, si)))
    in
    let (ctxt2, e2') =
      (let (t_i, si) = check_bool_expr ctxt1 e2 in (ctxt1, (t_i, si)))
    in
    let (ctxt3, e3') =
      (let (t_i, si) = check_expr ctxt2 e3 in (ctxt2, (t_i, si)))
    in
    let (_, ret_t, st') = check_stmt ctxt3 st
    in
    (ctxt, ret_t, SFor(s1', e2', e3', st'))
  | While(e, s) ->
    let e' = check_bool_expr ctxt e in
    let (_, rt, s') = check_stmt (StringMap.empty::ctxt) s in
    (ctxt, rt, SWhile(e', s') )
  | If (e, st1, st2) ->
    let e' = check_bool_expr ctxt e
    in
    let ifctxt = StringMap.empty::ctxt in
    let (_, rt1, st1') = check_stmt ifctxt st1
    in
    let (_, rt2, st2') = check_stmt ifctxt st2
    in
    let rt = match rt1, rt2 with
        (Void, _) -> Void
      | (_, Void) -> Void
      | (t1, t2) -> check_asn t1 t2
    in
    (ctxt, rt, SIf(e', st1', st2'))
  | _ -> (ctxt, Void, SExpr(Void, SNoexpr))

let def_ctxt =
  let add_func ctxt (name, func_t) = add_to_ctxt func_t name ctxt in
  List.fold_left add_func [StringMap.empty] builtinsFunc

let check_program (prog : stmt list) =
  if List.exists (fun x -> match x with Return(_) -> true | _ -> false) prog
  then raise (Failure "illegal return statement")
  else
    let (_, _, ssl) = check_stmt_list def_ctxt prog in ssl
