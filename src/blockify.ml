open Scanner
open Parser
open Ast
open Errors

let get_attr attr xml_obj =
    let attr_list = xml_obj.attributes in
        if List.exists (fun x -> x.aname = attr) attr_list
        then -> List.find (fun x -> x.aname = attr) attr_list
        else block_error xml_obj 
            ("Attribute '" ^ attr ^ "' missing")

let get_blks blkname xml_list =
    List.filter (fun x -> x.blkname = blkname) xml_list

let rec find_ref_blk xml_list ref_list =
    match ref_list with
          []    -> block_error xml_list "Empty reference list"
        | [h]   -> let blks = get_blks h xml_list in
                    match blks with
                        []  -> find_ref xml_list [] (* Shortcut for same err above *)
                      | [b] -> 

let parse_tree xml_tree =
    let blocktype = xml_tree.blkname 
    and blockname = in
        match blocktype with
              "BLOCK" -> blockify xml_tree
            | block_error xml_tree
                "Root element is not of type BLOCK"

let rec blockify xml_obj =
    let blocktype = xml_obj.blkname in
        match blocktype with
              "BLOCK"       ->
                  { name        = get_attr  "name"      xml_obj;
                    inputs      = get_blks  "INPUT"     xml_obj.inner_objs;
                    outputs     = get_blks  "OUTPUT"    xml_obj.inner_objs;
                    inner_blks  = List.map  blockify    xml_obj.inner_objs
                  } 
            | "REFERENCE"   -> 
                let ref = get_attr "ref" xml_obj in
                if ref.reftype <> "FILE"
                then block_error xml_obj
                    "Reference block does not reference a file"
                else
                    (* Parse the referenced file and then search 
                     * for the block referenced recursively *) 
                    let vlin = open_in ref.refroot in
                    let lexbuf = Lexing.from_channel vlin in
                    let xml_tree = Parser.xml_tree Scanner.token lexbuf in

                  { name        = get_attr  "name"      xml_obj;
                    inputs      = get_blks  "INPUT"     xml_obj.inner_objs;
                    outputs     = get_blks  "OUTPUT"    xml_obj.inner_objs;
                    ref_blk     = List.map  blockify    xml_obj.inner_objs
                  } 
            | "CONNECTION"  -> ()
            | "INPUT"       -> ()
            | "OUTPUT"      -> ()
            | "CONSTANT"    -> 
            | "SIGNAL"      -> 
            | "CAST"        -> 
            | "MEM"         -> 
            | "DT"          -> 
            | "NOT"         -> 
            | "AND"         -> 
            | "OR"          -> 
            | "NOR"         -> 
            | "NAND"        -> 
            | "XOR"         -> 
            | "XNOR"        -> 
            | "BITWISE"     -> 
            | "IF"          -> 
            | "COMPARE"     -> 
            | "SUM"         -> 
            | "PROD"        -> 
            | "GAIN"        -> 
            | "INV"         -> 
            | "MUX"         -> 
            | "DEMUX"       -> 
            | "STRUCT"      -> 
            | "CONSTRUCT"   -> 
            | "MAP"         -> 
            | "FILTER"      -> 
            | "REDUCE"      -> 

(* Type containing all accepted blocks *)
let blocks = 
      block
    | reference
    | constant
    | signal
    | atomic_parts
    | array_parts
    | struct_parts
    | func_parts

let atomic_parts =
      cast
    | mem
    | dt
    | gate
    | bitwise
    | if_sw
    | compare
    | sum
    | prod
    | gain
    | inv

(* Block type definitions *)
let block = {
    name            : string;
    inputs          : blocks list;
    outputs         : blocks list;
    inner_blocks    : blocks list
}

let reference = {
    name            : string;
    inputs          : blocks list;
    outputs         : blocks list;
    ref_block       : block
}


