(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type slval =
  SLId of string
| SLArrEl of sarrEl
and
sarrEl =
  SOneDim of string * sexpr
| SMultiDim of sarrEl * sexpr
and
sexpr = typ * sx
and sx =
    SLiteral of int
  | SFliteral of string
  | SBoolLit of bool
  | SId of string
  | SStrLit of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of slval * sexpr
  | SCall of string * sexpr list
  | SFExpr of sfexpr
  | SNoexpr
  | SArrEl of sarrEl
  | SSeq of sexpr list
  | SNodeExpr of sexpr * sexpr
  | SEdgeExpr of sexpr * sexpr * sexpr
  | SClosure of sclsr
and sfexpr = {
  sname : string;
  styp : typ;
  sparams: bind list;
  sbody : sstmt;
}
and sclsr = {
  ind: int;
  free_vars: bind list;
}
and sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SVDecl of typ * string * sexpr option

type sprogram = sstmt list

(* type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    slocals : bind list;
    sbody : sstmt list;
  }

type sprogram = bind list * sfunc_decl list *)


(* Pretty-printing functions *)

let rec string_of_sstmt = function
    SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n"
  | SIf(e, s, SBlock([])) ->
      "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SVDecl (t, id, v) -> (string_of_typ t) ^ " " ^ id ^ " = " ^ (
    match v with
        None -> ""
      | Some(e) -> string_of_sexpr e) ^ ";\n"
and
string_of_sarrel = function
    SOneDim(v, idx) -> v ^ "[" ^ string_of_sexpr idx ^ "]"
    | SMultiDim(_, _) -> raise(Failure "multidim")
  (* | MultiDim(ae, idx) -> (string_of_sarrel ae) ^ string_of_int idx *)
and
string_of_slval = function
    SLId(v) -> v
  | SLArrEl(v) -> string_of_sarrel v
and
string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ ": " ^ (match e with
    SLiteral(l) -> string_of_int l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SFliteral(l) -> l
  | SId(s) -> s
  | SStrLit(s) -> "\"" ^ s ^ "\""
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SAssign(v, e) -> string_of_slval v ^ " = " ^ string_of_sexpr e
  | SCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SNoexpr -> ""
  | SArrEl(a) ->
      (match a with
      | SOneDim(s, e) -> s ^ "[" ^ string_of_sexpr e ^ "]"
      | SMultiDim(aa, e) -> string_of_sarrel aa ^ "[" ^ string_of_sexpr e ^ "]" )
  | SSeq(s) -> "[" ^ String.concat "," (List.map string_of_sexpr s) ^ "]"
  | SNodeExpr(idx,v) -> string_of_sexpr idx ^ "@" ^ string_of_sexpr v
  | SEdgeExpr(ndf, w, ndt) -> string_of_sexpr ndf ^ "->" ^ string_of_sexpr ndt ^ "->" ^ string_of_sexpr ndt
  | SFExpr(e) -> "function<" ^ string_of_params e.sparams ^ "; " ^ string_of_typ e.styp ^ ">\n" ^ string_of_sstmt e.sbody
  | SClosure(clsr) ->
      "{ ind: " ^ string_of_int clsr.ind ^ ", fvs: ("
      ^ (string_of_params clsr.free_vars) ^ ") } )"
  ) ^ ")"


let string_of_sprogram l =
  let stmts = List.map string_of_sstmt l in
  String.concat "\n" stmts


(* let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n" *)

(* let string_of_sprogram (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs) *)
