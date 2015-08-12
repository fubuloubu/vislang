open Xscanner
open Xparser
open Blockify
open Blockparse
open Compile

type action = BlockTree | Compile | DebugCode
type rwfile = File | StdIO

let _ =
    let action = if Array.length Sys.argv > 1 then
        List.assoc Sys.argv.(1) [ ("-b", BlockTree);
                                  ("-c", Compile); 
                                  ("-d", DebugCode)]
    else Compile
    and rwfile = if (Array.length Sys.argv > 2) then File else StdIO in

    let filein =
        match rwfile with
            File  -> (open_in Sys.argv.(2))
          | StdIO -> stdin
     in
    let lexbuf = Lexing.from_channel filein in
    let xml_tree = Xparser.xml_tree Xscanner.token lexbuf in
    let block_tree = Blockify.parse_xml_tree xml_tree in
    let program = Blockparse.block_parse block_tree in
    let listing =
        match action with
              BlockTree -> Blockparse.print_list program
            | Compile   -> Compile.translate program
            | DebugCode -> Compile.gen_debug_code program
     in match rwfile with
            File  -> output_string 
                        (open_out 
                            (Str.global_replace (Str.regexp "\.vl") ".c" Sys.argv.(2))
                        )
                        listing
          | StdIO -> print_string listing
