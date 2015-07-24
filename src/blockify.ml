open Scanner
open Parser
open Ast
open Errors

let get_attr attr xml_obj =
    let attr_list = xml_obj.attributes in
        if List.exists (fun x -> x.aname = attr) attr_list
        then List.find (fun x -> x.aname = attr) attr_list
        else block_error xml_obj 
            ("Attribute '" ^ attr ^ "' missing")

let get_blks blkname xml_list =
    List.filter (fun x -> x.blkname = blkname) xml_list

let rec find_ref_blk xml_list ref_list =
    match ref_list with
          []    -> block_error xml_list
                    "Empty reference list"
        | [h]   -> let blks = get_blks h xml_list in
                    match blks with
                        []  -> find_ref xml_list [] (* Shortcut for same err above *)
                      | [b] -> b 

let parse_tree xml_tree =
    let blocktype = xml_tree.blkname in
        match blocktype with
              "BLOCK" -> blockify xml_tree
            | _ -> block_error xml_tree
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
            | "CONSTANT"    -> ()
            | "SIGNAL"      -> ()
            | "CAST"        -> ()
            | "MEM"         -> ()
            | "DT"          -> ()
            | "NOT"         -> ()
            | "AND"         -> ()
            | "OR"          -> ()
            | "NOR"         -> ()
            | "NAND"        -> ()
            | "XOR"         -> ()
            | "XNOR"        -> ()
            | "BITWISE"     -> ()
            | "IF"          -> ()
            | "COMPARE"     -> ()
            | "SUM"         -> ()
            | "PROD"        -> ()
            | "GAIN"        -> ()
            | "INV"         -> ()
            | "MUX"         -> ()
            | "DEMUX"       -> ()
            | "STRUCT"      -> ()
            | "CONSTRUCT"   -> ()
            | "MAP"         -> ()
            | "FILTER"      -> ()
            | "REDUCE"      -> ()

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

let cast = {
    name            : string;
    input           : block;
    output          : block;
    dtype           : string
}

let atomic_parts =
      Cast of cast
    | Mem of mem
    | Dt of dt
    | Gate of gate
    | Bw of bitwise
    | IF of if_sw
    | Comp of compare
    | Sum of sum
    | Prod of prod
    | Gain of gain
    | Inv of inv

(* Type containing all accepted blocks *)
let blocks = 
      Block of block
    | Ref of reference
    | Const of constant
    | Sig of signal
    | Atom of atomic_parts
    | Array of array_parts
    | Struct of struct_parts
    | Func of func_parts
