; ModuleID = 'GMAIL'
source_filename = "GMAIL"

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%g\0A\00"
@fmt.3 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.4 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.5 = private unnamed_addr constant [4 x i8] c"%g\0A\00"

declare i8* @init_list()

declare i8* @list_push_left(i8*, i8*)

declare i8* @list_push_right(i8*, i8*)

declare i8* @list_pop_left(i8*)

declare i8* @list_pop_right(i8*)

declare i8* @list_peek_left(i8*)

declare i8* @list_peek_right(i8*)

declare i32 @list_length(i8*)

declare i8* @list_copy(i8*)

declare i8* @list_remove_by_index(i8*, i32)

declare i8* @init_node()

declare i32 @node_get_index(i8*)

declare i8* @node_get_value(i8*)

declare i8* @node_get_edges(i8*)

declare void @node_set_index(i8*, i32)

declare void @node_set_value(i8*, i8*)

declare void @assign_node(i8*, i32, i8*)

declare i8* @node_to_s(i8*, i32)

declare void @node_set_flag(i8*, i32)

declare i32 @node_get_flag(i8*)

declare i8* @init_edge()

declare i8* @edge_get_from(i8*)

declare i8* @edge_get_to(i8*)

declare i8* @edge_get_weight(i8*)

declare i8* @edge_set_from(i8*, i8*)

declare i8* @edge_set_to(i8*, i8*)

declare i8* @edge_set_weight(i8*, i8*)

declare i8* @edge_to_s(i8*, i32)

declare i8* @assign_edge(i8*, i8*, i8*, i8*)

declare i8* @init_graph()

declare i8* @graph_copy(i8*)

declare void @graph_add_node(i8*, i8*)

declare i32 @graph_add_edge(i8*, i8*)

declare i32 @graph_add_edge_by_node(i8*, i8*, i8*, i8*)

declare i8* @graph_get_node(i8*, i32)

declare i32 @graph_add_edge_by_id(i8*, i32, i32, i8*)

declare void @graph_remove_node(i8*, i8*)

declare void @graph_remove_edge(i8*, i8*)

declare i8* @graph_to_s(i8*, i32, i32)

declare void @graph_set_flags(i8*, i32)

define i32 @main() {
entry:
  %malloccall = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32))
  %env = bitcast i8* %malloccall to i8**
  store i8* null, i8** %env
  %env_p = load i8*, i8** %env
  %malloccall1 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2) to i32))
  %main = bitcast i8* %malloccall1 to { i32 ()*, i8* }*
  %tmp__ = insertvalue { i32 ()*, i8* } { i32 ()* @main, i8* null }, i8* %env_p, 1
  store { i32 ()*, i8* } %tmp__, { i32 ()*, i8* }* %main
  %malloccall2 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2) to i32))
  %f = bitcast i8* %malloccall2 to { i32 (i8*, i32, i32)*, i8* }*
  %malloccall3 = tail call i8* @malloc(i32 0)
  %tmp_ = bitcast i8* %malloccall3 to {}*
  store {} zeroinitializer, {}* %tmp_
  %env_p4 = bitcast {}* %tmp_ to i8*
  %tmp__5 = insertvalue { i32 (i8*, i32, i32)*, i8* } { i32 (i8*, i32, i32)* @f0, i8* null }, i8* %env_p4, 1
  store { i32 (i8*, i32, i32)*, i8* } %tmp__5, { i32 (i8*, i32, i32)*, i8* }* %f
  %malloccall6 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2) to i32))
  %sum = bitcast i8* %malloccall6 to { i32 (i8*, i32, i32)*, i8* }*
  %f7 = load { i32 (i8*, i32, i32)*, i8* }, { i32 (i8*, i32, i32)*, i8* }* %f
  store { i32 (i8*, i32, i32)*, i8* } %f7, { i32 (i8*, i32, i32)*, i8* }* %sum
  %sum8 = load { i32 (i8*, i32, i32)*, i8* }, { i32 (i8*, i32, i32)*, i8* }* %sum
  %fp = extractvalue { i32 (i8*, i32, i32)*, i8* } %sum8, 0
  %envp = extractvalue { i32 (i8*, i32, i32)*, i8* } %sum8, 1
  %_result = call i32 %fp(i8* %envp, i32 3, i32 4)
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i32 %_result)
  ret i32 0
}

define i32 @f0(i8* %env, i32 %a, i32 %b) {
entry:
  %malloccall = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %a1 = bitcast i8* %malloccall to i32*
  store i32 %a, i32* %a1
  %malloccall2 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %b3 = bitcast i8* %malloccall2 to i32*
  store i32 %b, i32* %b3
  %malloccall4 = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32))
  %env5 = bitcast i8* %malloccall4 to i8**
  store i8* %env, i8** %env5
  %env_p = load i8*, i8** %env5
  %malloccall6 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2) to i32))
  %f = bitcast i8* %malloccall6 to { i32 (i8*, i32, i32)*, i8* }*
  %tmp__ = insertvalue { i32 (i8*, i32, i32)*, i8* } { i32 (i8*, i32, i32)* @f0, i8* null }, i8* %env_p, 1
  store { i32 (i8*, i32, i32)*, i8* } %tmp__, { i32 (i8*, i32, i32)*, i8* }* %f
  %a7 = load i32, i32* %a1
  %b8 = load i32, i32* %b3
  %tmp = add i32 %a7, %b8
  ret i32 %tmp
}

declare i32 @printf(i8*, ...)

declare i8* @concat(i8*, i8*)

declare noalias i8* @malloc(i32)
