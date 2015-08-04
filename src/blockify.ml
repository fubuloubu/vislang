open Xst
open Errors
open Parts
(* Helper functions for Object instantiaion *)
let get_attr attribute xml_obj = 
    let attr = List.filter (fun x -> x.aname = attribute) xml_obj.attributes in
        match attr with
            []      -> object_error ("No attribute named " ^ attribute )
          | [a]     -> a.avalue
          | _ :: _  -> object_error ("Too many attributes named " ^ attribute)

let get_num_connections xml_obj =
    let inputs = List.filter
                 (fun x -> x.tagname = "CONNECTION")
                 xml_obj.inner_objs
     in List.length inputs

let get_connection input_to xml_obj =
    let input_from = List.filter (fun x -> (get_attr "to" x) = Name input_to)
        (List.filter (fun x -> x.tagname = "CONNECTION") xml_obj.inner_objs) in
        match input_from with
            []      -> object_error
                        ("No connections found for " ^ 
                            Xst.string_of_value (get_attr "name" xml_obj)
                        )
          | [cnx]   -> Xst.string_of_value (get_attr "from" cnx )
          | _ :: _  -> object_error 
                        ("Too many connections defined for " ^ 
                            Xst.string_of_value (get_attr "name" xml_obj)
                        )

let get_datatype dtype =
    match dtype with
        "single"
      | "auto"   -> "float_t" (* Assume single if unspecified *)
      | _ as d   -> d ^ "_t"

(* Structure for returning input and output types *)
type interface = {
    name     : string;
    datatype : string;
}

(* virtual Base class all blocks inherit from *)
class virtual base xml_obj = object
    val name : string   = Xst.string_of_value (get_attr "name" xml_obj)
    method name         = name
    method virtual get_inputs   : interface list
    method virtual get_outputs  : interface list
    method virtual print_class  : string
    method virtual print_obj    : string
end;;

(* Block class: BLOCK tag
 * inherits from base, is a container for other blocks *)
class block blockify xml_obj = object (self)
    inherit base xml_obj
    val inner_objs = List.map 
            (blockify 
                (Xst.string_of_value (get_attr "name" xml_obj))
            ) 
            (xml_obj.inner_objs)
    method inner_objs   = inner_objs
    method get_inputs   = List.map
        (fun x -> List.hd ((x :> base) #get_outputs))
        (List.filter 
            (fun (x : base) -> ((x :> base) #print_class) = "input")
            inner_objs
        )
    method get_outputs  = List.map
        (fun x -> List.hd ((x :> base) #get_inputs))
        (List.filter 
            (fun (x : base) -> ((x :> base) #print_class) = "output")
            inner_objs
        )
    method print_class  = "block"
    method input_type   = "struct " ^ name ^ "_inputs"
    method output_type  = "struct " ^ name ^ "_outputs"
    method input_struct = self#input_type ^ " {\n\t" ^
                        (String.concat ";\n\t" 
                            (List.map 
                            (fun x -> (get_datatype x.datatype) ^ " " ^ x.name)
                            self#get_inputs)
                        ) ^ ";\n};\n\n" 
    method output_struct = self#output_type ^ " {\n\t" ^
                        (String.concat ";\n\t" 
                            (List.map 
                            (fun x -> (get_datatype x.datatype) ^ " " ^ x.name)
                            self#get_outputs)
                        ) ^ ";\n};\n\n"
    method header     = "/* I/O Structures for block " ^ name ^ " */\n" ^
                        self#input_struct ^ self#output_struct ^
                        "/* Begin block " ^ name ^ " */\n" ^
                        self#output_type ^ " " ^ name ^ "(" ^ self#input_type ^ 
                        " inputs) {\n" ^ 
                        "\t/* Inputs for block " ^ name ^ " */\n\t" ^ 
                        (String.concat ";\n\t" 
                            (List.map 
                            (fun x -> (get_datatype x.datatype) ^ " " ^ 
                                x.name ^ " = inputs." ^ x.name
                            ) 
                            self#get_inputs)
                        ) ^ ";\n\n"
    method trailer    = "\t/* Outputs for block " ^ name ^" */\n\t" ^
                        "struct " ^ name ^ "_outputs outputs;\n\t" ^
                        (String.concat ";\n\t" 
                            (List.map 
                            (fun x -> "outputs." ^ x.name ^ " = " ^ x.name) 
                            self#get_outputs)
                        ) ^ ";\n\n" ^
                        "\treturn outputs;\n}\n" ^
                        "/* End block " ^ name ^ " */\n"
    method interface  = ""
    method print_obj  = "{\n  \"block\": {\n" ^
                        "    \"name\":\"" ^ name ^ "\"\n" ^
                        "    \"inner_objs\": [\n      " ^
                        (String.concat "\n      "
                            (List.map 
                                (fun (x : base) -> (x :> base) #print_obj) 
                                inner_objs
                            )
                        ) ^ "\n    ]" ^ 
                        "\n  }\n}\n"
end;;

(* virtual I/O Part class: do all I/O Part attributes and checking *)
class virtual io_part xml_obj = object (self)
    inherit base xml_obj
    val scope    = Xst.string_of_value (get_attr "scope"    xml_obj)
    val datatype = Xst.string_of_value (get_attr "datatype" xml_obj)
    method datatype = datatype
    val size     =                      get_attr "size"     xml_obj
    method get_inputs   = [{ name = self#name; datatype = self#datatype }]
    method get_outputs  = [{ name = self#name; datatype = self#datatype }]
    method print_obj    = "\"" ^ self#print_class ^ "\": { " ^
                          "\"name\":\"" ^ name ^ "\", " ^
                          "\"scope\":\"" ^ scope ^ "\", " ^
                          "\"size\":\"" ^ Xst.string_of_value (size) ^ "\" }"
end;;

(* Input class: INPUT tag*)
class input xml_obj = object (self)
    inherit io_part xml_obj as super
    method get_inputs   = object_error "Should never access inputs of input obj"
    method print_class  = "input"
end;;

(* Output class: OUTPUT tag *)
class output xml_obj = object (self)
    inherit io_part xml_obj as super
    method get_outputs  = object_error "Should never access outputs of output obj"
    method print_class  = "output"
end;;

(* Constant class: CONSTANT tag*)
class constant xml_obj = object (self)
    inherit input xml_obj (* A constant acts like an input, except it has 
                           * a value and doesn't interact with block I/O *)
    val value    = Xst.string_of_value (get_attr "value"    xml_obj)
    method value = value
    method print_class  = "constant"
    method print_obj    = "\"" ^ self#print_class ^ "\": { " ^
                          "\"name\":\"" ^ name ^ "\", " ^
                          "\"value\":\"" ^ value ^ "\", " ^
                          "\"size\":\"" ^ Xst.string_of_value (size) ^ "\"" ^
                          " }"
end;;

(* Memory class: MEM tag*)
class memory xml_obj = object (self)
    inherit base xml_obj as super
    val init_cond       = Xst.string_of_value (get_attr "ic" xml_obj)
    method init_cond = init_cond
    method print_class  = "memory"
    method print_obj    = "\"memory\": { " ^
                          "\"name\":\"" ^ name ^ "\", " ^
                          "\"init_cond\":" ^ init_cond ^ "\"" ^
                          " }"
end;;

(* Main block management functions *)
(* Blockify goes through and matches the tagname to the appropiate object *)
let rec blockify xml_obj = 
    match xml_obj.tagname with
          "BLOCK"   -> new block blockify xml_obj
          (* Note: passing blockify into block instantiation because it can't 
           * see at compile time what the function blockify is referring to *)
        | "INPUT"   -> new input       xml_obj
        | "OUTPUT"  -> new output      xml_obj
        | "CONSTANT"-> new constant    xml_obj
        | "MEM"     -> new memory      xml_obj
        | _         -> (partify xml_obj :> part)

(* Main caller function simply to protect against top level blocks not being
 * of type BLOCK *)
let parse_xml_tree xml_obj = 
    match xml_obj.tagname with
          "BLOCK"   -> blockify "" xml_obj
        | _ as name -> object_error ("Tag " ^ name ^ " cannot be top level block")
