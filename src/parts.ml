open Blockify

(* All parts inherit from this one *)
class virtual part xml_obj = object (self)
    method typedef_code = String.concat
                        ";\n"
                        (List.map
                            (fun x -> x.datatype ^ " " ^ 
                                self#name ^ "_" ^ x.name
                            )
                            self#get_outputs
                        ) ^ ";\n"
    method virtual body_code : string
end;;

(* NOT Gate Part class: unary NOT operation *)
class not_gate xml_obj = object (self)
    inherit part xml_obj
    val mutable inputs  = [{ name = "current"; datatype = "auto" }]
    method get_inputs   = inputs
    val mutable outputs = [{ name = "stored"; datatype = "auto" }]
    method get_outputs  = outputs
    method print_class = "not"
    method print_obj    = "\"" ^ self#print_class ^ "\": { " ^
                          "\"name\":\"" ^ name ^ "\", " ^
                          "\"operation\":\"!\" }"
end;;

(* virtual Binary Operation class: do all binary attributes and checking *)
class virtual binop_part xml_obj = object (self)
    inherit part xml_obj
    val virtual operation : string
    method operation = operation
    val mutable inputs  = [{ name = "current"; datatype = "boolean" }]
    method get_inputs   = inputs
    val mutable outputs = [{ name = "stored"; datatype = "auto" }]
    method get_outputs  = outputs
    method print_obj    = "\"" ^ self#print_class ^ "\": { " ^
                          "\"name\":\"" ^ name ^ "\", " ^
                          "\"operation\":\"" ^ self#operation ^ "\" }"
end;;

(* OR gate: inherits from binary_gate_part, logical OR operation *)
class or_gate xml_obj = object (self)
    inherit binop_part xml_obj
    val operation = "||"
    method print_class = "or"
end;;

(* AND gate: inherits from binary_gate_part, logical AND operation *)
class and_gate xml_obj = object (self)
    inherit binop_part xml_obj
    val operation = "&&"
    method print_class = "and"
end;;

(* Summation point: inherits from binop_part, addition operation *)
class sum xml_obj = object (self)
    inherit binop_part xml_obj
    val operation = "+"
    method print_class = "sum"
end;;

(* Production point: inherits from binop_part, multiplication operation *)
class prod xml_obj = object (self)
    inherit binop_part xml_obj
    val operation = "*"
    method print_class = "prod"
end;;

let partify xml_obj =
    match xml_obj.tagname with
        | "NOT"     -> (new not_gate    xml_obj :> part)
        | "AND"     -> (new and_gate    xml_obj :> part)
        | "OR"      -> (new or_gate     xml_obj :> part)
        | "SUM"     -> (new sum         xml_obj :> part)
        | "PROD"    -> (new prod        xml_obj :> part)
        (* CONNECTION blocks are not supported by this operation. 
         * See get_connection above *)
        | _ as name -> object_error ("Tag " ^ name ^ " not supported.")
