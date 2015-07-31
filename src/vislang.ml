open Xscanner
open Xparser
open Blockify
open Bytecode
open Optimize
open Compile

type action = BlockTree | Optimize | Compile

let _ =
    let action = if Array.length Sys.argv > 1 then
        List.assoc Sys.argv.(1) [ ("-b", BlockTree);
                                  ("-o", Optimize);
                                  ("-c", Compile) ]
    else Optimize in

    let lexbuf = Lexing.from_channel stdin in
    let xml_tree = Xparser.xml_tree Xscanner.token lexbuf in
    let block_tree = Blockify.parse_xml_tree xml_tree in

    match action with
          BlockTree -> let listing = block_tree#print_obj
                        in print_string listing
        | Compile   -> let program = Bytecode.block_parse block_tree
                        in let generated_code = Compile.translate program
                            in print_endline generated_code
        | Optimize  -> let program = Bytecode.block_parse block_tree
                        in let program = Optimize.optimize program
                        in let generated_code = Compile.translate program
                            in print_endline generated_code
