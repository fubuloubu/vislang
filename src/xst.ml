(* Abstract Syntax Tree Definition *)
type copr = Grt | Lst | Eqt | Neq | Leq | Geq           (* Comparison operators     *)
type bopr = Or | And | Not | Xor | Nand | Nor | Xnor    (* Bitwise operators        *)

type ref  = {
    reftype     :  string;
    refroot     :  string;
    reflist     :  string list;
}

type value = 
      Ref       of ref              (* List of strings leading to a block           *)
    | Name      of string           (* Name of a block                              *)
    | Int       of int              (* Standard int type                            *)
    | Float     of float            (* Standard float type                          *)
    | Bool      of bool             (* Standard boolean type                        *)
    | Scope     of string           (* global or local scope                        *)
    | Datatype  of string           (* datatype from set of types                   *)
    | Compopr   of copr             (* Comparision operator                         *)
    | Bitwopr   of bopr             (* Bitwise operator                             *)
    | Size      of int              (* Array size                                   *)

type attr = {
    aname       :  string;          (* Attribute Name                               *)
    avalue      :  value;           (* Attrbiute Value                              *)
}

type xml_obj = {
    tagname     :  string;          (* Block Name                                   *)
    attributes  :  attr list;       (* Dictionary of attribute names and values     *)
    inner_objs  :  xml_obj list;    (* List of contained XML objects (can be empty) *)
}

(* Helper functions for printing XML AST *)
let string_of_bitw_opr v = match v with
      Or    -> "or"
    | And   -> "and"
    | Not   -> "not"
    | Xor   -> "xor"
    | Nand  -> "nand"
    | Nor   -> "nor"
    | Xnor  -> "xnor"

let string_of_comp_opr v = match v with
      Grt   -> ">"
    | Lst   -> "<"
    | Eqt   -> "=="
    | Neq   -> "!="
    | Leq   -> "<="
    | Geq   -> ">="

let string_of_ref (v) =
      v.refroot ^ "|" ^ String.concat "|" (v.reflist) ^ " (" ^ v.reftype ^ " REF)"

let string_of_value value = match value with
      Ref      v -> string_of_ref v
    | Name     v -> v
    | Int      v -> string_of_int v
    | Float    v -> string_of_float v
    | Bool     v -> string_of_bool v
    | Scope    v -> v
    | Datatype v -> v
    | Compopr  v -> string_of_comp_opr v
    | Bitwopr  v -> string_of_bitw_opr v
    | Size     v -> string_of_int v

let string_of_attr (a) =
    a.aname ^ ": " ^ string_of_value a.avalue

let rec string_of_xml (obj) =
    "Block: " ^ obj.tagname ^ "\n" ^
    "Attributes:\n-" ^ 
    (String.concat "\n-" (List.map string_of_attr obj.attributes)) ^
    if obj.inner_objs == []
    then "\n"
    else
    "\n\nChildren:\n" ^
    (String.concat "\n" (List.map string_of_xml obj.inner_objs)) ^
    "\nEnd of Children for: " ^ obj.tagname ^"\n"
