; ModuleID = 'GMAIL'
source_filename = "GMAIL"

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%g\0A\00"
@str = private unnamed_addr constant [4 x i8] c"abc\00"
@str.3 = private unnamed_addr constant [7 x i8] c"q < 10\00"
@str.4 = private unnamed_addr constant [8 x i8] c"q >= 10\00"
@fmt.5 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.6 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.7 = private unnamed_addr constant [4 x i8] c"%g\0A\00"

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
  %malloccall2 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %p = bitcast i8* %malloccall2 to i32*
  store i32 7, i32* %p
  %malloccall3 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %q = bitcast i8* %malloccall3 to i32*
  store i32 99, i32* %q
  %malloccall4 = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32))
  %a = bitcast i8* %malloccall4 to i8**
  store i8* getelementptr inbounds ([4 x i8], [4 x i8]* @str, i32 0, i32 0), i8** %a
  %malloccall5 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2) to i32))
  %foo = bitcast i8* %malloccall5 to { void (i8*, double, i8*)*, i8* }*
  %malloccall6 = tail call i8* @malloc(i32 0)
  %tmp_ = bitcast i8* %malloccall6 to {}*
  store {} zeroinitializer, {}* %tmp_
  %env_p7 = bitcast {}* %tmp_ to i8*
  %tmp__8 = insertvalue { void (i8*, double, i8*)*, i8* } { void (i8*, double, i8*)* @f0, i8* null }, i8* %env_p7, 1
  store { void (i8*, double, i8*)*, i8* } %tmp__8, { void (i8*, double, i8*)*, i8* }* %foo
  %malloccall9 = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32))
  %l = bitcast i8* %malloccall9 to i8**
  %init_list = call i8* @init_list()
  %malloccall10 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %primitive_ptr = bitcast i8* %malloccall10 to i32*
  %void_ptr = bitcast i32* %primitive_ptr to i8*
  %list_pupulate = call i8* @list_push_right(i8* %init_list, i8* %void_ptr)
  %malloccall11 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %primitive_ptr12 = bitcast i8* %malloccall11 to i32*
  %void_ptr13 = bitcast i32* %primitive_ptr12 to i8*
  %list_pupulate14 = call i8* @list_push_right(i8* %init_list, i8* %void_ptr13)
  %malloccall15 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %primitive_ptr16 = bitcast i8* %malloccall15 to i32*
  %void_ptr17 = bitcast i32* %primitive_ptr16 to i8*
  %list_pupulate18 = call i8* @list_push_right(i8* %init_list, i8* %void_ptr17)
  %malloccall19 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %primitive_ptr20 = bitcast i8* %malloccall19 to i32*
  %void_ptr21 = bitcast i32* %primitive_ptr20 to i8*
  %list_pupulate22 = call i8* @list_push_right(i8* %init_list, i8* %void_ptr21)
  store i8* %init_list, i8** %l
  %foo23 = load { void (i8*, double, i8*)*, i8* }, { void (i8*, double, i8*)*, i8* }* %foo
  %fp = extractvalue { void (i8*, double, i8*)*, i8* } %foo23, 0
  %envp = extractvalue { void (i8*, double, i8*)*, i8* } %foo23, 1
  %l24 = load i8*, i8** %l
  call void %fp(i8* %envp, double 3.500000e+00, i8* %l24)
  %q25 = load i32, i32* %q
  %tmp = icmp slt i32 %q25, 10
  br i1 %tmp, label %then, label %else

merge:                                            ; preds = %else, %then
  ret i32 0

then:                                             ; preds = %entry
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.1, i32 0, i32 0), i8* getelementptr inbounds ([7 x i8], [7 x i8]* @str.3, i32 0, i32 0))
  br label %merge

else:                                             ; preds = %entry
  %printf26 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.1, i32 0, i32 0), i8* getelementptr inbounds ([8 x i8], [8 x i8]* @str.4, i32 0, i32 0))
  br label %merge
}

define void @f0(i8* %env, double %a, i8* %b) {
entry:
  %malloccall = tail call i8* @malloc(i32 ptrtoint (double* getelementptr (double, double* null, i32 1) to i32))
  %a1 = bitcast i8* %malloccall to double*
  store double %a, double* %a1
  %malloccall2 = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32))
  %b3 = bitcast i8* %malloccall2 to i8**
  store i8* %b, i8** %b3
  %malloccall4 = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32))
  %env5 = bitcast i8* %malloccall4 to i8**
  store i8* %env, i8** %env5
  %env_p = load i8*, i8** %env5
  %malloccall6 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2) to i32))
  %foo = bitcast i8* %malloccall6 to { void (i8*, double, i8*)*, i8* }*
  %tmp__ = insertvalue { void (i8*, double, i8*)*, i8* } { void (i8*, double, i8*)* @f0, i8* null }, i8* %env_p, 1
  store { void (i8*, double, i8*)*, i8* } %tmp__, { void (i8*, double, i8*)*, i8* }* %foo
  %a7 = load double, double* %a1
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.7, i32 0, i32 0), double %a7)
  %b8 = load i8*, i8** %b3
  %malloccall9 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %primitive_ptr = bitcast i8* %malloccall9 to i32*
  %void_ptr = bitcast i32* %primitive_ptr to i8*
  %list_push_right_result = call i8* @list_push_right(i8* %b8, i8* %void_ptr)
  %b10 = load i8*, i8** %b3
  %list_length_result = call i32 @list_length(i8* %b10)
  %printf11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.5, i32 0, i32 0), i32 %list_length_result)
  ret void
}

declare i32 @printf(i8*, ...)

declare i8* @concat(i8*, i8*)

declare noalias i8* @malloc(i32)
