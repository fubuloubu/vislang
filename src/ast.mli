type opr = Grt | Lst | Eqt | Neq | Leq | Geq (* Comparison operators            *)

type value = 
      Name of string                (* ID of variable                           *)
    | File of string                (* File name                                *)
    | Ref of string list            (* Singly-linked list leading to a block    *)
    | Int of int                    (* Standard int type                        *)
    | Float of float                (* Standard float type                      *)
    | Bool of bool                  (* Standard boolean type                    *)
    | Scope of string               (* global or local scope                    *)
    | Datatype of string            (* datatype from set of types               *)
    | Opr of opr                    (* Comparision operator                     *)
    | Size of int                   (* Array size                               *)

type attr = {
    aname   : string;               (* Attribute Name                           *)
    avalue  : value;                (* Attrbiute Value                          *)
}

type xml_obj = {
    tagname     : string;           (* Block Name                               *)
    attributes  : attr list;        (* Dictionary of attribute names and values *)
    inner_objs  : xml_obj list;     (* A tree is an XML object that contains    *)
}                                   (* more XML objects                         *)
