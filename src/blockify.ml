open Scanner
open Parser
open Ast
open Errors

let get_attr attr xml_obj =
    let attr_list = xml_obj.attributes in
        if List.exists (fun x -> x.aname = attr) attr_list
        then x.avalue in x = List.find (fun x -> x.aname = attr) attr_list
        else block_error xml_obj 
            ("Attribute '" ^ attr ^ "' missing")

let get_blks blkname xml_list =
    List.filter (fun x -> x.blkname = blkname) xml_list
(*
let rec find_ref_blk xml_list ref_list =
    match ref_list with
          []    -> block_error xml_list
                    "Empty reference list"
        | [h]   -> let blks = get_blks h xml_list in
                    match blks with
                        []  -> find_ref xml_list [] (* Shortcut for same err above *)
                      | [b] -> b 
*)
(* Get the matching input connection for the prescribed input *)
let get_input input_name xml_obj =
    let cnx = List.filter
            (fun x -> (get_attr "to" x) = input_name) 
            (get_blks "CONNECTION" xml_obj.inner_objs)
        in
        if List.length cnx <> 1
        then block_error xml_obj "No unique connections to block input"
        else get_attr "from" (List.nth cnx 1)

(* Block type definitions *)
type input = {
    name            : string;
    scope           : string;
    size            : int;
    dtype           : string
}

and output = {
    name            : string;
    scope           : string;
    size            : int;
    dtype           : string;
    input           : string
}

and block = {
    name            : string;
    inputs          : input list;
    outputs         : output list;
    inner_blocks    : any_block list
}
(*
and reference = {
    name            : string;
    inputs          : input list;
    outputs         : output list;
    ref_block       : block
}

and cast = {
    name            : string;
    input           : string;
    dtype           : string
}

and atomic_parts =
      Cast      of cast
    | Mem       of mem
    | Dt        of dt
    | Gate      of gate
    | Bw        of bitwise
    | IF        of if_sw
    | Comp      of compare
    | Sum       of sum
    | Prod      of prod
    | Gain      of gain
    | Inv       of inv
*)
(* Type containing all accepted blocks *)
and any_block = 
      Block     of block
(*  | Ref       of reference
    | Const     of constant
    | Sig       of signal
    | Atom      of atomic_parts
    | Array     of array_parts
    | Struct    of struct_parts
    | Func      of func_parts
*)
(* Blockification function *)
let rec blockify xml_obj =
    let blocktype = xml_obj.blkname in
        match blocktype with
              "BLOCK"       ->
                  { 
                    name        = get_attr  "name"      xml_obj;
                    inputs      = get_blks  "INPUT"     xml_obj.inner_objs;
                    outputs     = get_blks  "OUTPUT"    xml_obj.inner_objs;
                    inner_blocks  = List.map  blockify    xml_obj.inner_objs
                  } 
            | "REFERENCE"   -> ()(*
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

                  { 
                    name        = get_attr  "name"      xml_obj;
                    inputs      = get_blks  "INPUT"     xml_obj.inner_objs;
                    outputs     = get_blks  "OUTPUT"    xml_obj.inner_objs;
                    ref_blk     = List.map  blockify    xml_obj.inner_objs
                  } *)
            | "CONNECTION"  -> ()
            | "INPUT"       ->
                  { 
                    name        = get_attr  "name"      xml_obj;
                    scope       = get_attr  "scope"     xml_obj;
                    size        = string_to_int (get_attr  "size" xml_obj);
                    dtype       = get_attr  "type"      xml_obj
                  } 
            | "OUTPUT"      ->
                  { 
                    name        = get_attr  "name"      xml_obj;
                    scope       = get_attr  "scope"     xml_obj;
                    inputs      = get_input (get_attr "name" xml_obj) xml_obj;
                    size        = string_to_int (get_attr  "size" xml_obj);
                    dtype       = get_attr  "type"      xml_obj
                  } 
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

let parse_tree xml_tree =
    let blocktype = xml_tree.blkname in
        match blocktype with
              "BLOCK" -> blockify xml_tree
            | _ -> block_error xml_tree
                    "Root element is not of type BLOCK"
