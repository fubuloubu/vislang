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
    let block_tree = Blockify.parse_tree xml_tree in

    match action with
          Ast       -> let listing = block_tree#print_obj
                        in print_string listing
        | Bytecode  -> let listing = Bytecode.string_of_block_tree block_tree
                        in print_endline listing
        | Compile   -> let program = Bytecode.parse_block_tree block_tree
                        in let listing = Compile.translate program
                            in print_endline listing
