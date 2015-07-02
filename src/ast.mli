type opr = Grt | Lst | Eqt | Neq | Leq | Geq (* Comparison operators *)

type floating_point = Single of float | Double of float

type integer =
      Uint32 of int
    | Uint16 of int
    | Uint8  of int
    |  Int32 of int
    |  Int16 of int
    |  Int8  of int

type value = 
      File of string                (* File name *)
    | Ref of string * string list   (* Singly-linked list leading to a block *)
    | Int of integer                (* Storing multiple types of ints *)
    | Float of floating_point       (* Storing multiple types of floats *)
    | Bool of bool                  (* Standard boolean type *)
    | Scope of string               (* global or local scope *)
    | Datatype of string            (* datatype from set of types *)
    | Opr of opr                    (* Comparision operator *)
    | Size of int                   (* Array size *)

type attr = {
    aname   : string;               (* Attribute Name *)
    avalue  : value;                (* Attrbiute Value *)
}

type block = {
    bname   : string;               (* Block Name *)
    btype   : string;               (* Block Type *)
    attrs   : attr list;            (* Dictionary of attribute names and values *)
    gcode   : string;               (* Generated Code *)
    inputs  : string * string list; (* Dictionary of block inputs and connections *)
    outputs : string list;          (* List of block outputs *)
}

type system = block list (* A system is a list of blocks *)
