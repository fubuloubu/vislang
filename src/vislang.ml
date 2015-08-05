open Xscanner
open Xparser
open Blockify
open Blockparse
open Optimize
open Compile

type action = BlockTree | Compile | Optimize | DebugCode

let _ =
    let action = if Array.length Sys.argv > 1 then
        List.assoc Sys.argv.(1) [ ("-b", BlockTree);
                                  ("-c", Compile); 
                                  ("-o", Optimize);
                                  ("-d", DebugCode)]
    else Optimize in
    
    let lexbuf = Lexing.from_channel stdin in
    let xml_tree = Xparser.xml_tree Xscanner.token lexbuf in
    let block_tree = Blockify.parse_xml_tree xml_tree in
    let program = Blockparse.block_parse block_tree in
    let listing =
        match action with
              BlockTree -> Blockparse.print_list program
            | Compile   -> Compile.translate program
            | Optimize  -> let program = Optimize.optimize program
                            in Compile.translate program
            | DebugCode -> Compile.gen_debug_code program
     in print_string listing
