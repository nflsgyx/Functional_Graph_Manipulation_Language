(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Concat

type uop = Neg | Not

type typ = Int | Bool | Float | Void | Empty
          | Array of typ * int | String
          | Lst of typ | Edge of typ | Node of typ | Graph of typ * typ
          | WildCard of int
          | Func of func_typ
and func_typ = {
  param_typs: typ list;
  return_typ: typ;
}


type bind = typ * string

type lval =
  LId of string
| LArrEl of arrEl
and
arrEl =
    OneDim of string * expr
  | MultiDim of arrEl * expr
and
expr =
    Literal of int
  | Fliteral of string
  | BoolLit of bool
  | StrLit of string
  | Id of string
  | ArrEl of arrEl
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of lval * expr
  | Call of string * expr list
  | FExpr of fexpr
  | CMCall of string * expr list
  | Noexpr
  | Seq of expr list
  | NodeExpr of expr * expr
  | EdgeExpr of expr * expr * expr
and fexpr = {
  name : string;
  typ : typ;
  params: bind list;
  body : stmt;
} and stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | VDecl of typ * string * expr option


(* type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt;
  } *)


type program = stmt list


(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Concat -> "^"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Void -> "void"
  | String -> "string"
  | Array(typ, _) -> string_of_typ typ
  | Lst(typ) -> "list<" ^ string_of_typ typ ^ ">"
  | Empty -> ""
  | WildCard(wid) -> "wcard" ^ string_of_int wid
  | Node(typ) -> "node<" ^ string_of_typ typ ^ ">"
  | Edge(typ) -> "edge<" ^ string_of_typ typ ^ ">"
  | Graph(typ1, typ2) -> "graph<" ^ string_of_typ typ1 ^ ", " ^ string_of_typ typ2 ^ ">"
  | Func(e) -> "func<" ^ (String.concat "," (List.map string_of_typ e.param_typs)) ^ "; " ^ (string_of_typ e.return_typ) ^ ">"
  | _ -> "not implemented"


let string_of_params l =
  let string_of_p = function
    (t, n) -> ("(" ^ string_of_typ t ^ "," ^ n ^ ")") in
  "[" ^ (String.concat ";" (List.map string_of_p l)) ^ "]"


let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | VDecl (t, id, v) -> (string_of_typ t) ^ " " ^ id ^ " = " ^ (
    match v with
        None -> ""
      | Some(e) -> string_of_expr e) ^ ";\n"
and
string_of_arrel = function
    OneDim(v, idx) -> v ^ "[" ^ string_of_expr idx ^ "]"
  | MultiDim(ae, idx) -> (string_of_arrel ae) ^ string_of_expr idx
and
string_of_lval = function
    LId(v) -> v
  | LArrEl(v) -> string_of_arrel v
and
string_of_expr = function
    Literal(l) -> string_of_int l
  | Fliteral(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | StrLit(s) -> "\"" ^ s ^ "\""
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> string_of_lval v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | CMCall(f, hd::el) ->
      string_of_expr hd ^ "." ^ f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | CMCall(_, _) -> raise (Failure "no possible")
  | Noexpr -> ""
  | ArrEl(a) ->
      (match a with
      | OneDim(s, e) -> s ^ "[" ^ string_of_expr e ^ "]"
      | MultiDim(aa, e) -> string_of_arrel aa ^ "[" ^ string_of_expr e ^ "]" )
  | Seq(s) -> "[" ^ String.concat "," (List.map string_of_expr s) ^ "]"
  | NodeExpr(idx,v) -> string_of_expr idx ^ "@" ^ string_of_expr v
  | EdgeExpr(ndf, w, ndt) -> string_of_expr ndf ^ "->" ^ string_of_expr w ^ "->" ^ string_of_expr ndt
  | FExpr(e) -> "function<" ^ string_of_params e.params ^ "; " ^ string_of_typ e.typ ^ ">\n" ^ string_of_stmt e.body


(* let string_of_vdecl = function
  | (Array(t, n), id) -> string_of_typ t ^ " " ^ id ^ "[" ^ string_of_int n ^ "]" ^ ";\n"
  | (t, id) -> string_of_typ t ^ " " ^ id ^ ";\n" *)

(* let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n" *)

let string_of_program l =
  let stmts = List.map string_of_stmt l in
  String.concat "\n" stmts


(* let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
 *)



