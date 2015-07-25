open Scanner
open Parser
open Blockify
open Compile
open Bytecode

type action = Ast | Interpret | Bytecode | Compile

let _ =
    let action = if Array.length Sys.argv > 1 then
        List.assoc Sys.argv.(1) [ ("-a", Ast);
                                  ("-b", Bytecode);
                                  ("-c", Compile) ]
    else Compile in

    let lexbuf = Lexing.from_channel stdin in
    let xml_tree = Parser.xml_tree Scanner.token lexbuf in
    let program = Blockify.parse_tree xml_tree in

    match action with
          Ast       -> let listing = Ast.string_of_xml xml_tree
                        in print_string listing
        | Bytecode  -> let listing = Bytecode.string_of_prog
                                    (Compile.translate program)
                        in print_endline listing
        | Compile   -> print_endline (Compile.translate program)
