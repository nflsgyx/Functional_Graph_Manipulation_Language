(*Top-level of the GMAIL compiler: scan & parse the input,
   check the resulting AST and generate an SAST from it, generate LLVM IR,
   and dump the module *)

type action = Ast | Sast | Lsast | LLVM_IR | Compile

let () =
  let action = ref Compile in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
    ("-ls", Arg.Unit (set_action Lsast), "Print the LSAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in
  let usage_msg = "usage: ./gmail.native [-a|-s|-l|-c] [file.mc]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;



  let lexbuf = Lexing.from_channel !channel in
  let ast = Gmailparse.program Scanner.token lexbuf in
  match !action with
    Ast ->  print_endline ("AST:\n"); print_string (Ast.string_of_program ast)
    | _ ->
  let sast = Semant.check_program ast in
  match !action with
    Sast ->
      print_endline ("AST:\n");
      print_string (Ast.string_of_program ast);
      print_endline ("\n=======================================\nSAST:\n");
      print_endline (Sast.string_of_sprogram sast)
   | _ ->
  let lsast = Lift.lift sast in
   match !action with
    Lsast ->
      print_endline ("AST:\n");
      print_string (Ast.string_of_program ast);
      print_endline ("\n=======================================\nSAST:\n");
      print_endline (Sast.string_of_sprogram sast);
      print_endline ("\n=======================================\nLSAST:\n");
      print_endline (Lift.string_of_lsast lsast)
    | _ ->
  match !action with
    LLVM_IR ->
    print_endline ("AST:\n");
    print_string (Ast.string_of_program ast);
    print_endline ("\n=======================================\nSAST:\n");
    print_endline (Sast.string_of_sprogram sast);
    print_endline ("\n=======================================\nLSAST:\n");
    print_endline (Lift.string_of_lsast lsast);
    print_endline ("\n=======================================\nLLVM_IR:\n");
    let m = Codegen.translate lsast in
    Llvm_analysis.assert_valid_module m;
    print_endline (Llvm.string_of_llmodule m)
  |_->
    let m = Codegen.translate lsast in
    Llvm_analysis.assert_valid_module m;
    print_string (Llvm.string_of_llmodule m)


