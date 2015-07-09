type copr = Grt | Lst | Eqt | Neq | Leq | Geq           (* Comparison operators     *)
type bopr = Or | And | Not | Xor | Nand | Nor | Xnor    (* Bitwise operators        *)

type ref  = {
    reftype     :  string;
    refroot     :  string;
    reflist     :  string list;
}

type value = 
      Ref       of ref              (* List of strings leading to a block           *)
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
    blkname     :  string;          (* Block Name                                   *)
    attributes  :  attr list;       (* Dictionary of attribute names and values     *)
    inner_objs  :  xml_obj list;    (* List of contained XML objects (can be empty) *)
}
