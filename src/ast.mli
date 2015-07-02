type opr = Grt | Lst | Eqt | Neq | Leq | Geq (* Comparison operators *)

type value = 
      File of string                (* File name *)
    | Ref of string * string list   (* Singly-linked list leading to a block *)
    | Int of int                    (* Standard int type *)
    | Float of float                (* Standard float type *)
    | Bool of bool                  (* Standard boolean type *)
    | Scope of string               (* global or local scope *)
    | Datatype of string            (* datatype from set of types *)
    | Opr of opr                    (* Comparision operator *)
    | Size of int                   (* Array size *)

type attr = {
    aname   : string;               (* Attribute Name *)
    avalue  : value;                (* Attrbiute Value *)
}

type block_def = {
    bname   : string;               (* Block Name *)
    btype   : string;               (* Block Type *)
    attrs   : attr list;            (* Dictionary of attribute names and values *)
    gcode   : string;               (* Generated Code *)
    inputs  : string * string list; (* Dictionary of block inputs and connections *)
    outputs : string list;          (* List of block outputs *)
}

type connection = string * string   (* Connections type: block input and connection *)
type block =
       Root of block_def list (* A block can be a list of block defs *)
     | Leaf of block_def      (* or a block def itself *)
