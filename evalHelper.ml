module L = Llvm
module A = Ast
open Sast
open Ast
open Lift

module StringMap = Map.Make(String)

let context    = L.global_context ()
let the_module = L.create_module context "GMAIL"
(* Get types from the context *)
let i32_t      = L.i32_type    context
and i8_t       = L.i8_type     context
and i1_t       = L.i1_type     context
and float_t    = L.double_type context
and void_t     = L.void_type   context
and str_t      = L.pointer_type (L.i8_type context)
and lst_t      = L.pointer_type (L.i8_type context)
and node_t     = L.pointer_type (L.i8_type context)
and edge_t     = L.pointer_type (L.i8_type context)
and graph_t    = L.pointer_type (L.i8_type context)
and void_ptr_t = L.pointer_type (L.i8_type context)
(* Return the LLVM type for a GMAIL type *)

let addr_t t = L.pointer_type t

let rec ltype_of_typ = function
    A.Int   -> i32_t
  | A.Bool  -> i1_t
  | A.Float -> float_t
  | A.String  -> str_t
  | A.Void  -> void_t
  | A.Array(typ, len) -> L.array_type (ltype_of_typ typ) len
  | A.Lst(_) -> lst_t
  | A.Node(_) -> node_t
  | A.Edge(_) -> edge_t
  | A.Graph(_) -> graph_t
  | A.Func(ftype) -> ltype_of_clsr_func "" ftype
  | _ as x-> raise(Failure ((string_of_typ x) ^ " NIP"))

and ltype_of_func name (ret_t : typ) param_ts =
    let param_types = (List.map ltype_of_typ param_ts) in
    let param_types =
      if name = "main" then param_types
      else void_ptr_t :: param_types
    in L.function_type (ltype_of_typ ret_t) (Array.of_list param_types)

and ltype_of_lfexpr name (lfexpr : lfunc) =
  ltype_of_func name lfexpr.lreturn_typ (List.map fst lfexpr.lparams)

and ltype_of_sfunction name (sfunc : func_typ) =
  ltype_of_func name sfunc.return_typ sfunc.param_typs

and ltype_of_clsr name lfexpr =
  let func_t = L.pointer_type (ltype_of_lfexpr name lfexpr) in
  L.struct_type context [|func_t; void_ptr_t|]

and ltype_of_clsr_func name (sfunc : func_typ) =
  let func_t = L.pointer_type (ltype_of_sfunction name sfunc) in
  L.struct_type context [|func_t; void_ptr_t|]

let is_primitive_typ = function
  | A.Int | A.Bool | A.Float | A.String | A.Void -> true
  | _ -> false

let is_primitive ll_typ =
  if ll_typ = i32_t || ll_typ = i8_t || ll_typ = i1_t || ll_typ = float_t then true
  else false

let ll_getaddr builder (v : L.llvalue) : L.llvalue =
    let ptr = L.build_malloc (L.type_of v) "primitive_ptr" builder in
    (* let _ = L.build_store v tmp builder in *)
    ptr

let ll_voidptr2wildcard builder (ptr : L.llvalue) (card_types : (int, L.lltype) Hashtbl.t) (wid: int) =
  let out_typ = Hashtbl.find card_types wid in
  if is_primitive out_typ then
  let ptr = L.build_bitcast ptr (addr_t out_typ) "wildcard_ptr" builder in
  L.build_load ptr "vptr2wildcard" builder
  else ptr

let ll_ptr2voidptr builder (ptr : L.llvalue) =
  L.build_bitcast ptr void_ptr_t "void_ptr" builder

let ll_get_void_addr builder v (ty:A.typ) =
   match ty with
  | A.WildCard(_) when is_primitive (L.type_of v) ->
    let addr = ll_getaddr builder v in
    ll_ptr2voidptr builder addr
  | _ -> v

let get_wildcard_types formals (args: sexpr list) : (int, L.lltype) Hashtbl.t =
  let update_wildcard_types (result : (int, L.lltype) Hashtbl.t) a (b : sexpr) =
    let _ = match a, (fst b) with
      A.WildCard(wid), ty -> Hashtbl.replace result wid (ltype_of_typ ty)
    | A.Lst(A.WildCard(wid)), A.Lst(ty) ->Hashtbl.replace result wid (ltype_of_typ ty)
    | A.Node(A.WildCard(wid)), A.Node(ty) ->  Hashtbl.replace result wid (ltype_of_typ ty)
    | A.Edge(A.WildCard(wid)), A.Edge(ty) -> Hashtbl.replace result wid (ltype_of_typ ty)
    | A.Graph(A.WildCard(wid1), A.WildCard(wid2)), A.Graph(ty1, ty2) ->
      let _ = Hashtbl.replace result wid1 (ltype_of_typ ty1) in
      Hashtbl.replace result wid2 (ltype_of_typ ty2)
    | _ -> ()
    in result
  in
  let len1 = List.length formals and len2 = List.length args in
  if len1 != len2 then
    raise (Failure (Printf.sprintf "different fold_left2 length: formals:%d args:%d"  len1 len2))
  else
    List.fold_left2 update_wildcard_types (Hashtbl.create 42) formals args


(* Node LLVM function declarations *)
let init_list_func_t       = L.function_type lst_t [||]
let init_list_func_f       = L.declare_function "init_list" init_list_func_t the_module

let list_push_left_func_t       = L.function_type lst_t [|lst_t; void_ptr_t|]
let list_push_left_func_f       = L.declare_function "list_push_left" list_push_left_func_t the_module
let list_push_right_func_t      = L.function_type lst_t [|lst_t; void_ptr_t|]
let list_push_right_func_f      = L.declare_function "list_push_right" list_push_right_func_t the_module
let list_pop_left_func_t        = L.function_type void_ptr_t [|lst_t|]
let list_pop_left_func_f        = L.declare_function "list_pop_left" list_pop_left_func_t the_module
let list_pop_right_func_t       = L.function_type void_ptr_t [|lst_t|]
let list_pop_right_func_f       = L.declare_function "list_pop_right" list_pop_right_func_t the_module
let list_peek_left_func_t       = L.function_type void_ptr_t [|lst_t|]
let list_peek_left_func_f       = L.declare_function "list_peek_left" list_peek_left_func_t the_module
let list_peek_right_func_t      = L.function_type void_ptr_t [|lst_t|]
let list_peek_right_func_f      = L.declare_function "list_peek_right" list_peek_right_func_t the_module
let list_length_func_t = L.function_type i32_t [|lst_t|]
let list_length_func_f = L.declare_function "list_length" list_length_func_t the_module
let list_copy_func_t = L.function_type lst_t [|lst_t|]
let list_copy_func_f = L.declare_function "list_copy" list_copy_func_t the_module
let list_remove_by_index_func_t = L.function_type void_ptr_t [|lst_t; i32_t|]
let list_remove_by_index_func_f = L.declare_function "list_remove_by_index" list_remove_by_index_func_t the_module

let init_node_func_t       = L.function_type node_t [||]
let init_node_func_f       = L.declare_function "init_node" init_node_func_t the_module
let node_get_index_func_t  = L.function_type i32_t [|node_t|]
let node_get_index_func_f  = L.declare_function "node_get_index" node_get_index_func_t the_module
let node_get_value_func_t  = L.function_type void_ptr_t [|node_t|]
let node_get_value_func_f  = L.declare_function "node_get_value" node_get_value_func_t the_module
let node_get_edges_func_t  = L.function_type lst_t [|node_t|]
let node_get_edges_func_f  = L.declare_function "node_get_edges" node_get_edges_func_t the_module
let node_set_index_func_t  = L.function_type void_t [|node_t; i32_t|]
let node_set_index_func_f  = L.declare_function "node_set_index" node_set_index_func_t the_module
let node_set_value_func_t  = L.function_type void_t [|node_t; void_ptr_t|]
let node_set_value_func_f  = L.declare_function "node_set_value" node_set_value_func_t the_module
let assign_node_func_t     = L.function_type void_t [|node_t; i32_t; void_ptr_t|]
let assign_node_func_f     = L.declare_function "assign_node" assign_node_func_t the_module
let node_to_s_func_t = L.function_type str_t [|node_t; i32_t|]
let node_to_s_func_f = L.declare_function "node_to_s" node_to_s_func_t the_module
let node_set_flag_func_t = L.function_type void_t [|node_t; i32_t|]
let node_set_flag_func_f = L.declare_function "node_set_flag" node_set_flag_func_t the_module
let node_get_flag_func_t = L.function_type i32_t [|node_t|]
let node_get_flag_func_f = L.declare_function "node_get_flag" node_get_flag_func_t the_module


(* Edge LLVM function declarations *)
let init_edge_func_t       = L.function_type edge_t [||]
let init_edge_func_f       = L.declare_function "init_edge" init_edge_func_t the_module
let edge_get_from_func_t   = L.function_type node_t [|edge_t|]
let edge_get_from_func_f   = L.declare_function "edge_get_from" edge_get_from_func_t the_module
let edge_get_to_func_t     = L.function_type node_t [|edge_t|]
let edge_get_to_func_f     = L.declare_function "edge_get_to" edge_get_to_func_t the_module
let edge_get_weight_func_t = L.function_type void_ptr_t [|edge_t|]
let edge_get_weight_func_f = L.declare_function "edge_get_weight" edge_get_weight_func_t the_module
let edge_set_from_func_t   = L.function_type void_ptr_t [|edge_t; node_t|]
let edge_set_from_func_f   = L.declare_function "edge_set_from" edge_set_from_func_t the_module
let edge_set_to_func_t     = L.function_type void_ptr_t [|edge_t; node_t|]
let edge_set_to_func_f     = L.declare_function "edge_set_to" edge_set_to_func_t the_module
let edge_set_weight_func_t = L.function_type void_ptr_t [|edge_t; void_ptr_t|]
let edge_set_weight_func_f = L.declare_function "edge_set_weight" edge_set_weight_func_t the_module
let edge_to_s_func_t = L.function_type str_t [|edge_t; i32_t|]
let edge_to_s_func_f = L.declare_function "edge_to_s" edge_to_s_func_t the_module
let assign_edge_func_t     = L.function_type void_ptr_t [|edge_t; node_t; node_t; void_ptr_t|]
let assign_edge_func_f     = L.declare_function "assign_edge" assign_edge_func_t the_module

(* Graph LLVM function declarations *)
let init_graph_func_t = L.function_type graph_t [||]
let init_graph_func_f = L.declare_function "init_graph" init_graph_func_t the_module
let graph_copy_func_t = L.function_type graph_t [|graph_t|]
let graph_copy_func_f = L.declare_function "graph_copy" graph_copy_func_t the_module
let graph_add_node_func_t = L.function_type void_t [|graph_t; node_t|]
let graph_add_node_func_f = L.declare_function "graph_add_node" graph_add_node_func_t the_module
let graph_add_edge_func_t = L.function_type i32_t [|graph_t; edge_t|]
let graph_add_edge_func_f = L.declare_function "graph_add_edge" graph_add_edge_func_t the_module
let graph_add_edge_by_node_func_t = L.function_type i32_t [|graph_t; node_t; node_t; void_ptr_t|]
let graph_add_edge_by_node_func_f = L.declare_function "graph_add_edge_by_node" graph_add_edge_by_node_func_t the_module
let graph_get_node_func_t = L.function_type node_t [|graph_t; i32_t|]
let graph_get_node_func_f = L.declare_function "graph_get_node" graph_get_node_func_t the_module
let graph_add_edge_by_id_func_t = L.function_type i32_t [|graph_t; i32_t; i32_t; void_ptr_t|]
let graph_add_edge_by_id_func_f = L.declare_function "graph_add_edge_by_id" graph_add_edge_by_id_func_t the_module
let graph_remove_node_func_t = L.function_type void_t [|graph_t; node_t|]
let graph_remove_node_func_f = L.declare_function "graph_remove_node" graph_remove_node_func_t the_module
let graph_remove_edge_func_t = L.function_type void_t [|graph_t; edge_t|]
let graph_remove_edge_func_f = L.declare_function "graph_remove_edge" graph_remove_edge_func_t the_module
let graph_to_s_func_t = L.function_type str_t [|graph_t; i32_t; i32_t|]
let graph_to_s_func_f = L.declare_function "graph_to_s" graph_to_s_func_t the_module
let graph_set_flags_func_t = L.function_type void_t [|graph_t; i32_t|]
let graph_set_flags_func_f = L.declare_function "graph_set_flags" graph_set_flags_func_t the_module


let builtin_func_map : (A.typ * L.llvalue) StringMap.t =
  let t = StringMap.empty in
  let t = StringMap.add "init_list" (Void, init_list_func_f) t in
  let t = StringMap.add "list_push_left" (Lst(WildCard(1)), list_push_left_func_f) t in
  let t = StringMap.add "list_push_right" (Lst(WildCard(1)), list_push_right_func_f) t in
  let t = StringMap.add "list_pop_left" (WildCard(1), list_pop_left_func_f) t in
  let t = StringMap.add "list_pop_right" (WildCard(1), list_pop_right_func_f) t in
  let t = StringMap.add "list_peek_left" (WildCard(1), list_peek_left_func_f) t in
  let t = StringMap.add "list_peek_right" (WildCard(1), list_peek_right_func_f) t in
  let t = StringMap.add "list_length" (Int, list_length_func_f) t in
  let t = StringMap.add "list_copy" (Lst(WildCard(1)), list_copy_func_f) t in
  let t = StringMap.add "list_remove_by_index" (WildCard(1), list_remove_by_index_func_f) t in
  let t = StringMap.add "init_node" (Node(WildCard(1)), init_node_func_f) t in
  let t = StringMap.add "node_get_index" (Int, node_get_index_func_f) t in
  let t = StringMap.add "node_get_value" (WildCard(1), node_get_value_func_f) t in
  let t = StringMap.add "node_get_edges" (Lst(Edge(WildCard(1))), node_get_edges_func_f) t in
  let t = StringMap.add "node_set_index" (Void, node_set_index_func_f) t in
  let t = StringMap.add "node_set_value" (Void, node_set_value_func_f) t in
  let t = StringMap.add "assign_node" (Void, assign_node_func_f) t in
  let t = StringMap.add "node_set_flag" (Void, node_set_flag_func_f) t in
  let t = StringMap.add "node_get_flag" (Int, node_get_flag_func_f) t in
  let t = StringMap.add "node_to_s" (String, node_to_s_func_f) t in
  let t = StringMap.add "init_edge" (Edge(WildCard(1)), init_edge_func_f) t in
  let t = StringMap.add "edge_get_from" (Node(WildCard(1)), edge_get_from_func_f) t in
  let t = StringMap.add "edge_get_to" (Node(WildCard(1)), edge_get_to_func_f) t in
  let t = StringMap.add "edge_get_weight" (WildCard(1), edge_get_weight_func_f) t in
  let t = StringMap.add "edge_set_from" (Void, edge_set_from_func_f) t in
  let t = StringMap.add "edge_set_to" (Void, edge_set_to_func_f) t in
  let t = StringMap.add "edge_set_weight" (Void, edge_set_weight_func_f) t in
  let t = StringMap.add "edge_to_s" (String, edge_to_s_func_f) t in
  let t = StringMap.add "assign_edge" (Void, assign_edge_func_f) t in
  let t = StringMap.add "init_graph" (Graph(WildCard(1), WildCard(2)), init_graph_func_f) t in
  let t = StringMap.add "graph_copy" (Graph(WildCard(1), WildCard(2)), graph_copy_func_f) t in
  let t = StringMap.add "graph_add_node" (Void, graph_add_node_func_f) t in
  let t = StringMap.add "graph_add_edge" (Int, graph_add_edge_func_f) t in
  let t = StringMap.add "graph_add_edge_by_node" (Int, graph_add_edge_by_node_func_f) t in
  let t = StringMap.add "graph_get_node" (Node(WildCard(1)), graph_get_node_func_f) t in
  let t = StringMap.add "graph_add_edge_by_id" (Int, graph_add_edge_by_id_func_f) t in
  let t = StringMap.add "graph_remove_node" (Void, graph_remove_node_func_f) t in
  let t = StringMap.add "graph_remove_edge" (Void, graph_remove_edge_func_f) t in
  let t = StringMap.add "graph_set_flags" (Void, graph_set_flags_func_f) t in
  let t = StringMap.add "graph_to_s" (String, graph_to_s_func_f) t in
  t

let build_lst_init builder =
  L.build_call init_list_func_f [||] "init_list" builder

let build_node_init builder =
  L.build_call init_node_func_f [||] "init_node" builder

let build_edge_init builder =
  L.build_call init_edge_func_f [||] "init_edge" builder

let build_graph_init builder =
  L.build_call init_graph_func_f [||] "init_graph" builder

let build_alloc builder typ name =
  let local_var = L.build_alloca (ltype_of_typ typ) name builder
  in match typ with
    A.Lst(_) ->
    let func_ret = L.build_call init_list_func_f [||] "init_list" builder in
    ignore (L.build_store func_ret local_var builder);
    local_var
  | A.Node(_) ->
    let func_ret = L.build_call init_node_func_f [||] "init_node" builder in
    ignore (L.build_store func_ret local_var builder);
    local_var
  | A.Edge(_) ->
    let func_ret = L.build_call init_edge_func_f [||] "init_edge" builder in
    ignore (L.build_store func_ret local_var builder);
    local_var
  | A.Graph(_) ->
    let func_ret = L.build_call init_graph_func_f [||] "init_graph" builder in
    ignore (L.build_store func_ret local_var builder);
    local_var
  | _ -> local_var


