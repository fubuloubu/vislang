open Xst
open Errors
(* Helper functions for Object instantiaion *)
let get_attr attribute xml_obj = 
    let attr = List.filter (fun x -> x.aname = attribute) xml_obj.attributes in
        match attr with
            []      -> object_error ("No attribute named " ^ attribute )
          | [a]     -> a.avalue
          | _ :: _  -> object_error ("Too many attributes named " ^ attribute)

let get_datatype dtype =
    match dtype with
        "single"
      | "auto"   -> "float_t" (* Assume single if unspecified *)
      | "boolean"-> "bool"
      | _ as d   -> d ^ "_t"

(* Structure for returning input and output types *)
type interface = {
    name     : string;
    datatype : string;
}

(* virtual Base class all blocks inherit from. All methods here
 * will be utilized by upstream utilities *)
class virtual base xml_obj = object
    val name : string   = Xst.string_of_value (get_attr "name" xml_obj)
    method name         = name
    method virtual inputs       : interface list
    method virtual outputs      : interface list
    method virtual inner_objs   : base list
    (* Potentially dangerous, but only used in context of 
     * getting inner objects first *)
    method virtual set_inner_objs : base list -> unit
    method virtual print_class  : string
    method virtual print_obj    : string
    method virtual header       : string
    method virtual body         : string
    method virtual trailer      : string
    method get_connection input_to =
        let input_from = List.filter (fun x -> (get_attr "to" x) = Name input_to)
            (List.filter (fun x -> x.tagname = "CONNECTION") xml_obj.inner_objs) in
            match input_from with
                []      -> object_error
                            ("No connections found for " ^ 
                                Xst.string_of_value (get_attr "name" xml_obj)
                            )
              | [cnx]   -> get_attr "from" cnx
              | _ :: _  -> object_error 
                            ("Too many connections defined for " ^ 
                                Xst.string_of_value (get_attr "name" xml_obj)
                            )
end;;

(* Block class: BLOCK tag
 * inherits from base, is a container for other blocks *)
class block blockify xml_obj = object (self)
    inherit base xml_obj
    val mutable inner_objs = List.map blockify xml_obj.inner_objs
    method inner_objs   = List.rev inner_objs
    method set_inner_objs new_inner_objs = inner_objs <- new_inner_objs
    method inputs   = List.map
        (fun x -> List.hd ((x :> base) #outputs))
        (List.filter 
            (fun (x : base) -> ((x :> base) #print_class) = "input")
            inner_objs
        )
    method outputs  = List.map
        (fun x -> List.hd ((x :> base) #inputs))
        (List.filter 
            (fun (x : base) -> ((x :> base) #print_class) = "output")
            inner_objs
        )
    method print_class  = "block"
    method input_type   = "struct " ^ name ^ "_in"
    method output_type  = "struct " ^ name ^ "_out"
    method input_struct = self#input_type ^ " {\n\t" ^
                        (String.concat ";\n\t" 
                            (List.map 
                            (fun x -> (get_datatype x.datatype) ^ " " ^ x.name)
                            self#inputs)
                        ) ^ ";\n};\n\n" 
    method output_struct = self#output_type ^ " {\n\t" ^
                        (String.concat ";\n\t" 
                            (List.map 
                            (fun x -> (get_datatype x.datatype) ^ " " ^ x.name)
                            self#outputs)
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
                            self#inputs)
                        ) ^ ";\n\n"
    method trailer    = "\t/* Outputs for block " ^ name ^" */\n\t" ^
                        self#output_type ^ " outputs;\n\t" ^
                        (String.concat ";\n\t" 
                            (List.map 
                            (fun x -> "outputs." ^ x.name ^ " = " ^ x.name) 
                            self#outputs)
                        ) ^ ";\n\n" ^
                        "\treturn outputs;\n}\n" ^
                        "/* End block " ^ name ^ " */\n"
    method body       = "\t" ^ String.concat "\t" 
                            (List.map
                                (fun x -> (x :> base) #body)
                                self#inner_objs
                            )
    method print_obj  = "{\n  \"block\": {\n" ^
                        "    \"name\":\"" ^ name ^ "\"\n" ^
                        "    \"inner_objs\": [\n      " ^
                        (String.concat "\n      "
                            (List.map 
                                (fun (x : base) -> (x :> base) #print_obj) 
                                self#inner_objs
                            )
                        ) ^ "\n    ]" ^ 
                        "\n  }\n}\n"
end;;

(* virtual I/O Part class: do all I/O Part attributes and checking *)
class virtual io_part xml_obj = object (self)
    inherit base xml_obj
    method inner_objs = object_error 
                            ("Should not try to access inner objects of " ^
                             self#print_class ^ " object: " ^ self#name ^ "")
    method set_inner_objs new_inner_objs = object_error
                            ("Should not try to set inner objects of " ^
                             self#print_class ^ " object: " ^ self#name ^ "")
    val datatype = Xst.string_of_value (get_attr "datatype" xml_obj)
    method datatype = datatype
    val size     =                      get_attr "size"     xml_obj
    method inputs   = [{ name = self#name; datatype = self#datatype }]
    method outputs  = [{ name = self#name; datatype = self#datatype }]
    method print_obj    = "\"" ^ self#print_class ^ "\": { " ^
                          "\"name\":\"" ^ name ^ "\", " ^
                          "\"size\":\"" ^ Xst.string_of_value (size) ^ "\" }"
    method header     = ""
    method body       = ""
    method trailer    = ""
end;;

(* Input class: INPUT tag*)
class input xml_obj = object (self)
    inherit io_part xml_obj as super
    method inputs   = object_error "Should never access inputs of input obj"
    method print_class  = "input"
end;;

(* Output class: OUTPUT tag *)
class output xml_obj = object (self)
    inherit io_part xml_obj as super
    method outputs  = object_error "Should never access outputs of output obj"
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

(* All parts inherit from this one *)
class virtual part xml_obj = object (self)
    inherit base xml_obj
    method inner_objs = object_error 
                            ("Should not try to access inner objects of " ^
                             self#print_class ^ " object: " ^ self#name ^ "")
    method set_inner_objs new_inner_objs = object_error
                            ("Should not try to set inner objects of " ^
                             self#print_class ^ " object: " ^ self#name ^ "")
    val virtual mutable inputs  : interface list
    method inputs  = inputs
    method set_inputs new_inputs = inputs <- new_inputs
    val virtual mutable outputs : interface list
    method outputs = outputs
    method set_outputs new_outputs = outputs <- new_outputs
    method virtual body         : string
    method header     = ""
    method trailer    = ""
end;;

(* Memory class: MEM tag*)
class memory xml_obj = object (self)
    inherit part xml_obj
    val init_cond       = Xst.string_of_value (get_attr "ic" xml_obj)
    val mutable inputs   = [{ name = "current"; datatype = "auto" }]
    val mutable outputs  = [{ name = "stored"; datatype = "auto" }]
    method init_cond = init_cond
    method print_class  = "memory"
    method print_obj    = "\"memory\": { " ^
                          "\"name\":\"" ^ name ^ "\", " ^
                          "\"init_cond\":" ^ init_cond ^ "\"" ^
                          " }"
    method body         = "\t" ^ (List.hd outputs).name ^ " = " ^ 
                          (List.hd inputs).name ^ "\n"
end;;

(* NOT Gate Part class: unary NOT operation *)
class not_gate xml_obj = object (self)
    inherit part xml_obj
    val mutable inputs  = [{ name = "input"; datatype = "boolean" }]
    val mutable outputs = [{ name = "output"; datatype = "boolean" }]
    method print_class  = "not"
    method print_obj    = "\"" ^ self#print_class ^ "\": { " ^
                          "\"name\":\"" ^ name ^ "\", " ^
                          "\"operation\":\"!\" }"
    method body         = "\t" ^ (get_datatype (List.hd outputs).datatype) ^ " " ^ 
                          (List.hd outputs).name ^ " = !(" ^ 
                          (List.hd inputs).name ^ ")\n"
end;;

let get_num_connections xml_obj =
    let inputs = List.filter
                 (fun x -> x.tagname = "CONNECTION")
                 xml_obj.inner_objs
     in List.length inputs

let get_cnx_list xml_obj set_type= 
    let num_cnx = get_num_connections xml_obj
     in 
        let rec create_cnx_list num_cnx cnx_list = 
            let idx = (num_cnx - (List.length cnx_list)) 
         in let idx_name = "input" ^ (string_of_int idx)
         in match idx with
                0 -> cnx_list
              | _ -> let cnx_list = {name = idx_name; datatype = set_type} :: cnx_list
                    in create_cnx_list num_cnx cnx_list
     in create_cnx_list num_cnx []
    (* inputs for binop parts are named input1 through inputN 
    * and the operation will be applied on all elements *)

(* virtual Binary Operation class: do all binary attributes and checking *)
class virtual binop_part xml_obj = object (self)
    inherit part xml_obj
    val virtual operation : string
    method operation = operation
    method print_obj    = "\"" ^ self#print_class ^ "\": { " ^
                          "\"name\":\"" ^ name ^ "\", " ^
                          "\"operation\":\"" ^ self#operation ^ "\" }"
    method body         = "\t" ^ (get_datatype (List.hd outputs).datatype) ^ " " ^ 
                          (List.hd outputs).name ^ " =  " ^ String.concat 
                                (" " ^ self#operation ^ " ")
                                (List.map 
                                    (fun x -> x.name) 
                                    self#inputs
                                ) ^ 
                          ";\n"
end;;

(* intermediate class to explicitly set datatype for gate parts *)
class virtual gate xml_obj = object
    inherit binop_part xml_obj as super
    val mutable inputs  = get_cnx_list xml_obj "boolean"
    val mutable outputs = [{ name = "output"; datatype = "boolean" }]
end;;

(* OR gate: inherits from binary_gate_part, logical OR operation *)
class or_gate xml_obj = object (self)
    inherit gate xml_obj
    val operation = "||"
    method print_class = "or"
end;;

(* AND gate: inherits from binary_gate_part, logical AND operation *)
class and_gate xml_obj = object (self)
    inherit gate xml_obj
    val operation = "&&"
    method print_class = "and"
end;;

(* Summation point: inherits from binop_part, addition operation *)
class sum xml_obj = object (self)
    inherit binop_part xml_obj
    val operation = "+"
    method print_class = "sum"
    val mutable inputs  = get_cnx_list xml_obj "auto"
    val mutable outputs = [{ name = "output"; datatype = "auto" }]
end;;

(* Production point: inherits from binop_part, multiplication operation *)
class prod xml_obj = object (self)
    inherit binop_part xml_obj
    val operation = "*"
    method print_class = "prod"
    val mutable inputs  = get_cnx_list xml_obj "auto"
    val mutable outputs = [{ name = "output"; datatype = "auto" }]
end;;

(* Main block management functions *)
(* Blockify goes through and matches the tagname to the appropiate object *)
let rec blockify xml_obj = 
    match xml_obj.tagname with
          "BLOCK"   -> (new block blockify xml_obj :> base)
          (* Note: passing blockify into block instantiation because it can't 
           * see at compile time what the function blockify is referring to *)
        | "INPUT"   -> (new input       xml_obj :> base)
        | "OUTPUT"  -> (new output      xml_obj :> base)
        | "CONSTANT"-> (new constant    xml_obj :> base)
        | "MEM"     -> (new memory      xml_obj :> base)
        | "NOT"     -> (new not_gate    xml_obj :> base)
        | "AND"     -> (new and_gate    xml_obj :> base)
        | "OR"      -> (new or_gate     xml_obj :> base)
        | "SUM"     -> (new sum         xml_obj :> base)
        | "PROD"    -> (new prod        xml_obj :> base)
        (* CONNECTION blocks are not supported by this operation. 
         * See get_connection above *)
        | _ as name -> object_error ("Tag " ^ name ^ " not supported.")

(* Main caller function simply to protect against top level blocks not being
 * of type BLOCK *)
let parse_xml_tree xml_obj = 
    match xml_obj.tagname with
          "BLOCK"   -> blockify xml_obj
        | _ as name -> object_error ("Tag " ^ name ^ " cannot be top level block")
