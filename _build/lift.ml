(* Authors: Sam Jayasinghe, Claire Adams, Cindy Le, Crystal Ren *)

open Ast
open Sast

module StringMap = Map.Make(String)

type environment = {
  variables: typ StringMap.t;
  parent: environment option;
}

(* Lifted function *)
type lfunc = {
  lname: string;
  lfvs: bind list; (* Free variables *)
  lreturn_typ: typ;
  lparams: bind list;
  lbody: sstmt;
}

(* For inbuilt functions, create empty function types *)
let built_in_decls =
  let empty_func ty = ({ return_typ = ty; param_typs = [];}) in
  let add_default map (name, ty) = StringMap.add name (Func ty) map in
  let builtins = List.map (fun (name, func_t) ->
      let is_func = match func_t with
          Func(func_t) -> (name, empty_func func_t.return_typ)
        | _ -> raise (Failure ("not a built-in function")) in
      is_func) Semant.builtinsFunc in
  List.fold_left add_default StringMap.empty builtins

let rec print_env e =
  print_endline "env start";
  let _ = match e.parent with
      None -> print_endline "no parent"
    | Some(_) -> print_endline "has parent"
  in
  print_l (StringMap.bindings e.variables)
and
print_l l = match  l with
 [] -> print_endline "env finish"
| (k,v)::tl -> print_endline ("(" ^ k ^ ", " ^string_of_typ v ^")"); print_l tl


let rec lookup (e : environment) name =
  try
    StringMap.find name e.variables
  with Not_found -> match e.parent with
      Some(parent) -> lookup parent name
    | None ->
      try StringMap.find name built_in_decls
      with Not_found -> raise (Failure ("Lift: undeclared identifier " ^ name))

let add_bind m (t, id) = StringMap.add id t m


let rec name_of_arrEl a  = match a with
  SOneDim(name, _) -> name
| SMultiDim(var, _) -> name_of_arrEl var

let rec dfs_sstmt funcs env sstmt =
  let (funcs', fvs', env', sstmt') =
    match sstmt with
    | SVDecl(lt, name, i) ->
      let (new_typ, funcs', fvs', opt_sexpr') = match i with
          None -> (lt, funcs, [], None)
        | Some(sexpr) ->
          let (funcs', fvs', sexpr') = dfs_sexpr funcs env sexpr ~fname:name in
          let (rt, _) = sexpr' in
          let new_typ = match (lt, rt) with
              Func(_), Func(_) -> lt
            | _ -> lt in
          (new_typ, funcs', fvs', Some(sexpr'))
      in
      let env' = {
        variables = StringMap.add name new_typ env.variables;
        parent = env.parent
      } in
      (* print_endline("vdecl"); *)
      (* print_l (StringMap.bindings env'.variables); *)
      (funcs', fvs', env', SVDecl(new_typ, name, opt_sexpr'))

    | SBlock(sl) ->
      let (funcs1, fvs1, env1, sl1) = dfs_sstmts funcs env sl in
      (funcs1, fvs1, env1, SBlock(sl1))

    | SReturn e ->
      let (funcs1, fvs1, e1) = dfs_sexpr funcs env e in
      (funcs1, fvs1, env, SReturn(e1))
    | SIf(e, s1, s2) ->
      let (funcs1, fvs1, e') = dfs_sexpr funcs env e in
      let (funcs2, fvs2, _, s1') = dfs_sstmt funcs1 env s1 in
      let (funcs3, fvs3, _, s2') = dfs_sstmt funcs2 env s2 in
      (funcs3, List.concat [fvs1; fvs2; fvs3], env, SIf(e', s1', s2'))
    | SExpr e ->
      let (funcs1, fvs1, e1) = dfs_sexpr funcs env e in
      (funcs1, fvs1, env, SExpr(e1))

    | SFor (s1, e1, e2, body) ->
      let (funcs1, fvs1, s1') =
        (let (funcs1', fvs1', s1'') = dfs_sexpr funcs env s1 in (funcs1', fvs1', s1''))
      in
      let (funcs2, fvs2, e1') =
        (let (funcs2', fvs2', e1'') = dfs_sexpr funcs1 env e1 in (funcs2',fvs2', e1''))
      in
      let (funcs3, fvs3, e2') = dfs_sexpr funcs2 env e2
      in
      let sl = match body with
        | SBlock(sl) -> sl
        | _ as x-> [x]
      in
      let (funcs4, fvs4, env_body, body') =
        List.fold_left
          (fun (funcs_curr, fvslist, env_curr, stmtlist) s ->
             let (funcs_lift, fvs_lift, env_lift, slift) =
               dfs_sstmt funcs_curr env_curr s in
             (funcs_lift, fvs_lift::fvslist, env_lift, slift::stmtlist)
          ) (funcs3, [fvs3; fvs2; fvs1], env, []) sl in
      (funcs4, List.concat (List.rev fvs4), env_body, SFor(s1', e1', e2', SBlock(body')))

    | SWhile(e, s) ->
      let (funcs1, fvs1, e') = dfs_sexpr funcs env e in
      let (funcs2, fvs2, _, s') = dfs_sstmt funcs1 env s in
      (funcs2, List.concat [fvs1; fvs2], env, SWhile(e', s') )
    in
  let check_scope (_, fv) = not (StringMap.mem fv env.variables) in
  let fvs' = List.filter check_scope fvs' in
  (funcs', fvs', env', sstmt')

and dfs_sstmts funcs env = function
    [] -> (funcs, [], env, [])
  | sstmt :: rest ->
    (* print_endline "dfs_sstmts ing"; *)
    let (funcs1, fvs1, env1, sstmts1) =
      dfs_sstmt funcs env sstmt in
    (* print_endline "dfs_sstmt done"; *)
    (* print_l (StringMap.bindings env1.variables); *)
    let new_env = {
      variables = List.fold_left add_bind env1.variables fvs1;
      parent = env1.parent
    } in
    (* print_l (StringMap.bindings env1.variables); *)
    let (funcs2, fvs2, env2, sstmts2) = dfs_sstmts funcs1 new_env rest in
    (funcs2, List.concat [fvs1; fvs2], env2, sstmts1::sstmts2)

and dfs_sexpr ?fname funcs env (t, expr) =
  let check_scope (_, fv) = not (StringMap.mem fv env.variables) in
  let (funcs', fvs', expr') = match expr with
      SFExpr(fexpr) ->
      let (funcs', fvs', (t, clsr)) = match fname with
          Some x -> build_closure funcs env fexpr ~fname:x
        | None -> build_closure funcs env fexpr in
      let fvs' = List.filter check_scope fvs' in
      (funcs', fvs', (t, SClosure(clsr)))

    | SAssign(var, e2) ->
      let (funcs', fvs2, e2') = dfs_sexpr funcs env e2 in
      let sname = match var with
        SLId v -> v
      | SLArrEl v -> name_of_arrEl v
      in
      let fvs1 =
        if StringMap.mem sname env.variables || StringMap.mem sname built_in_decls
        then []
        else [lookup env sname, sname]
      in
      (funcs', List.concat [fvs2; fvs1], (t, SAssign(var, e2')))



    | SNodeExpr(e1, e2) ->
      let (fncs1, fvs1, e1') = dfs_sexpr funcs env e1 in
      let (_, fvs2, e2') = dfs_sexpr fncs1 env e2 in
      (fncs1, List.concat [fvs1; fvs2], (t, SNodeExpr(e1', e2')))
    | SEdgeExpr(e1, e2, e3) ->
      let (fncs1, fvs1, e1') = dfs_sexpr funcs env e1 in
      let (fncs2, fvs2, e2') = dfs_sexpr fncs1 env e2 in
      let (fncs3, fvs3, e3') = dfs_sexpr fncs2 env e3 in
      (fncs3, List.concat [fvs1; fvs2; fvs3], (t, SEdgeExpr(e1', e2', e3')))






    | SId s1 ->
      let fv =
        if StringMap.mem s1 env.variables || StringMap.mem s1 built_in_decls
        then []
        else [lookup env s1, s1]
      in
      (funcs, fv, (t, SId(s1)))
    | SBinop(se1, op, se2) ->
      let (funcs1, fvs1, se1') = dfs_sexpr funcs env se1 in
      let (funcs2, fvs2, se2') = dfs_sexpr funcs1 env se2 in
      (funcs2, List.concat [fvs1; fvs2], (t, SBinop(se1', op, se2')))
    | SUnop(op, e1) ->
      let (funcs1, fvs1, e1') = dfs_sexpr funcs env e1 in
      (funcs1, fvs1, (t, SUnop(op, e1')))
    | SCall(sname, args) ->
       let fv' =
         if StringMap.mem sname env.variables || StringMap.mem sname built_in_decls
         then None
         else Some(lookup env sname, sname)
       in
       let (funcs1, fvs1, args') = dfs_sexprs funcs env (List.rev args) in
       let fvs' = match fv' with
           Some(x) -> x :: fvs1
         | _ -> fvs1
       in (funcs1, fvs', (t, SCall(sname, args')))
      (*  | _ ->
         (* Need this for recursion. *)
         let (funcs1, fvs1, _)
           = dfs_sexpr funcs env (lt, se) in
         let (funcs2, fvs2, args') = dfs_sexprs funcs1 env args in
         (funcs2, fvs1@fvs2, (t, SFCall((lt, se), args')))) *)
    | _ as x -> (funcs, [], (t, x))
  in
  let fvs' = List.filter check_scope fvs' in
  (funcs', fvs', expr')

and dfs_sexprs funcs env = function
    [] -> (funcs, [], [])
  | sexpr :: rest ->
    let (funcs1, fvs1, sexpr1) = dfs_sexpr funcs env sexpr in
    let new_env = {
      variables = List.fold_left add_bind env.variables fvs1;
      parent = env.parent;
    } in
    let (funcs2, fvs2, rest) = dfs_sexprs funcs1 new_env rest in
    (funcs2, List.concat [fvs1; fvs2], sexpr1 :: rest)

and build_closure ?fname funcs env fexpr =
  let vars = List.fold_left add_bind StringMap.empty fexpr.sparams in
  let name = match fname with Some x -> x | None -> "" in
  (* let vars_rec = match name with "" -> vars in *)
    (* | _ -> StringMap.add name SABSTRACT vars in *)
  let new_env = {
    variables = vars;
    parent = Some env
  } in
  let (funcs', fvs, _, body') = dfs_sstmt funcs new_env fexpr.sbody in
  let clsr = {
    ind = List.length funcs';
    free_vars = fvs;
  } in
  let new_func = {
    lname = name;
    lfvs = fvs;
    lreturn_typ = fexpr.styp;
    lparams = fexpr.sparams;
    lbody = body'
  } in
  let func_t = {
    param_typs = List.map fst fexpr.sparams;
    return_typ = fexpr.styp;
  } in
  (new_func :: funcs', fvs, (Func(func_t), clsr))

(* Lift takes a list of sast stmts, and converts to a list of (fname, func) *)
(* sstmt list -> (string * lfunc) list *)
let lift sstmts =
  let default_env = { variables = StringMap.empty; parent = None } in
  let (funcs, _, _, sstmts') = dfs_sstmts [] default_env sstmts in
  let main_func = {
    lname = "main";
    lfvs = [];
    lreturn_typ = Int;
    lparams = [];
    lbody = SBlock(sstmts')
  } in
  let name i func = ("f" ^ string_of_int i, func) in
  let named_funcs = List.mapi name (List.rev funcs) in
  (("main", main_func) :: named_funcs)

let string_of_lfunc f = String.concat "\n" [
    "-name: " ^ f.lname;
    " -fvs: " ^ String.concat ""
      (List.map (fun (t, n) -> "(" ^ (string_of_typ t) ^ ": " ^ n^ ") ") f.lfvs);
    " -return_t: " ^ string_of_typ f.lreturn_typ;
    " -params: " ^ String.concat ""
      (List.map (fun (t, n) -> "(" ^ (string_of_typ t) ^ ": " ^ n ^ ") ") f.lparams);
    " -lbody: \n" ^ string_of_sstmt f.lbody;
  ]

let helper (name, f) = name ^ ":\n" ^ (string_of_lfunc f)

let rec string_of_lsast = function
    [] -> ""
  | item :: rest -> String.concat "\n" [(helper item);(string_of_lsast rest)]
