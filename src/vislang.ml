open Xscanner
open Xparser
open Blockify
open Compile
open Bytecode

type action = Ast | Bytecode | Compile

let _ =
    let action = if Array.length Sys.argv > 1 then
        List.assoc Sys.argv.(1) [ ("-a", Ast);
                                  ("-b", Bytecode);
                                  ("-c", Compile) ]
    else Compile in

    let lexbuf = Lexing.from_channel stdin in
    let xml_tree = Xparser.xml_tree Xscanner.token lexbuf in

    match action with
          Ast       -> let listing = Xst.string_of_xml xml_tree
                        in print_string listing
        | Bytecode  -> let program = Blockify.parse_tree xml_tree in
                           let listing = Bytecode.string_of_prog program
                            in print_endline listing
        | Compile   -> let program = Blockify.parse_tree xml_tree in
                           let listing = Compile.translate program
                            in print_endline listing
