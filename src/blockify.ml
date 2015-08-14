open Xst
open Errors
(* Helper functions for Object instantiaion *)
let get_attr attribute xml_obj = 
    let attr = List.filter (fun x -> x.aname = attribute) xml_obj.attributes in
        match attr with
            []      -> object_error ("No attribute named " ^ attribute ^
                                     " in:\n" ^ (string_of_xml xml_obj))
          | [a]     -> a.avalue
          | _ :: _  -> object_error ("Too many attributes named " ^ attribute ^
                                     " in:\n" ^ (string_of_xml xml_obj))

let get_datatype dtype =
    match dtype with
        "boolean"   -> "bool"
      | "single"    -> "float_t"
      | _ as d      -> d ^ "_t" (* e.g. uint32_t, int8_t, etc. *)

let if_elements l printstr =
    if (List.length l) > 0
    then printstr
    else ""

(* Structure for returning input and output types *)
type interface = {
    name     : string;
    datatype : string;
}

(* virtual Base class all blocks inherit from. All methods here
 * will be utilized by upstream utilities *)
class virtual base xml_obj = object
    val name : string   = string_of_value (get_attr "name" xml_obj)
    method name         = name
    (* Block-specific functionality *)
    method virtual inputs       : interface list
    method virtual outputs      : interface list
    method virtual inner_objs   : base list
    (* Potentially dangerous, but only used in context of 
     * getting inner objects first *)
    method virtual set_inputs   : interface list -> unit
    method virtual set_outputs  : interface list -> unit
    method virtual set_inner_objs : base list -> unit
    (* Used for general purposes and to distinguish blocks *)
    method virtual print_class  : string
    method virtual print_obj    : string
    (* Code generation functions *)
    method virtual header       : string
    method virtual body         : string
    method virtual trailer      : string
    (* Function used in trace algorithm in order to find
     * connection from an input *)
    method get_connection input_to =
        let input_from = List.filter (fun x -> (get_attr "to" x) = Name input_to)
            (List.filter (fun x -> x.tagname = "CONNECTION") xml_obj.inner_objs)
         in match input_from with
                []      -> object_error
                            ("No connections found for " ^ 
                                string_of_value (get_attr "name" xml_obj)
                            )
              | [cnx]   -> get_attr "from" cnx
              | _ :: _  -> object_error 
                            ("Too many connections defined for " ^ 
                                string_of_value (get_attr "name" xml_obj)
                            )
end;;

(* Intermediate class used by both block and reference classes *)
class virtual blk_or_ref blockify xml_obj = object (self)
    inherit base xml_obj
    val mutable virtual inner_objs : base list
    method inner_objs = List.rev inner_objs
    (* Get input/output objects inside this object *)
    method inputs   = List.map
        (fun x -> List.hd ((x :> base) #outputs))
        (List.filter 
            (fun (x : base) -> ((x :> base) #print_class) = "input")
            inner_objs
        )
    method outputs  = List.map
        (fun x -> List.hd ((x :> base) #outputs))
        (List.filter 
            (fun (x : base) -> ((x :> base) #print_class) = "output")
            inner_objs
        )
    (* Since this object has a set of inputs we want to keep immutable
     * use the following construct such that we can print what the body
     * code needs without modifying the block's list of inputs/outputs *)
    val mutable connected_inputs = []
    method connected_inputs = connected_inputs
    method set_inputs new_inputs = connected_inputs <- new_inputs 
    method set_outputs a = object_error (
                                "Should not set outputs of " ^
                                self#print_class ^ " object")
    method input_type   = if_elements
                            self#inputs
                            ("struct " ^ self#func ^ "_in")
    method output_type  = if_elements
                            self#outputs
                            ("struct " ^ self#func ^ "_out")
    method virtual func : string (* Used because block cannot have
                                  * a different name, but reference can *)
    method body       = if_elements (* Create code for setting input structure *)
                            self#inputs
                            (self#input_type ^ " " ^ 
                             self#name ^ "_inputs = " ^ "{\n\t\t" ^ 
                                (String.concat 
                                    ",\n\t\t" 
                                    (List.map
                                        (fun (x, y) -> "." ^ x.name ^ 
                                                       " = " ^ y.name
                                        )
                                        (List.combine
                                            self#inputs
                                            self#connected_inputs
                                        )
                                    )
                                ) ^ "\n\t};\n\t"
                            ) ^
                        if_elements (* Create code for setting output struct *)
                            self#outputs
                           (self#output_type ^ " " ^
                            self#name ^ "_outputs =\n\t\t") ^
                        self#func ^ "(" ^ (* function call *)
                        if_elements (* Only apply inputs if block has inputs *)
                            self#inputs
                            (self#name ^ "_inputs") ^ 
                        ");"
    method print_obj  = "\"" ^ self#print_class ^ "\": {\n" ^
                        "    \"name\":\"" ^ name ^ "\"\n" ^
                        "    \"inner_objs\": [\n      " ^
                        (String.concat "\n      "
                            (List.map 
                                (fun (x : base) -> (x :> base) #print_obj) 
                                self#inner_objs
                            )
                        ) ^ "\n    ]" ^ 
                        "\n}\n"
end;;

(* Block class: BLOCK tag, is a container for other blocks *)
class block blockify xml_obj = object (self)
    inherit blk_or_ref blockify xml_obj
    val mutable inner_objs = List.map 
                             blockify 
                             (List.filter
                                (fun x -> x.tagname <> "CONNECTION")
                                xml_obj.inner_objs
                             )
    method func = name
    method set_inner_objs new_inner_objs = inner_objs <- new_inner_objs
    method ref_blks = List.filter 
            (fun (x : base) -> let c = ((x :> base) #print_class) in
                                 c = "reference"
            )
            inner_objs
    method print_inc = if_elements
                            self#ref_blks
                            (String.concat 
                                "\n" 
                                (List.map 
                                    (fun x -> (x :> base) #header)
                                    self#ref_blks
                                ) ^ "\n\n"
                            )
    method static_blks = List.filter 
            (fun (x : base) -> let c = ((x :> base) #print_class) in
                                 c = "memory"
                              || c = "constant"
                              || c = "dt"
            )
            inner_objs
    method print_static = if_elements 
                            self#static_blks
                            ("/* Initialize static variables */\n" ^
                             String.concat 
                                "\n" 
                                (List.map 
                                    (fun x -> (x :> base) #header)
                                    self#static_blks
                                ) ^ "\n\n"
                            )
    method print_class  = "block"
    method input_struct = if_elements
                            self#inputs
                            (self#input_type ^ " {\n\t" ^
                                (String.concat ";\n\t" 
                                    (List.map 
                                    (fun x -> (get_datatype x.datatype) ^ 
                                              " " ^ x.name
                                    )
                                    self#inputs)
                                ) ^ ";\n};\n\n"
                            )
    method output_struct = if_elements
                            self#outputs
                            (self#output_type ^ " {\n\t" ^
                                (String.concat ";\n\t" 
                                    (List.map 
                                    (fun x -> (get_datatype x.datatype) ^
                                              " " ^ x.name
                                    )
                                    self#outputs)
                                ) ^ ";\n};\n\n"
                            )
    method header     = (* Include statements for referenced files*)
                        self#print_inc ^ 
                        (* Structure definition for block *)
                        if_elements
                            (self# inputs @ self#outputs)
                            ("/* I/O Structures for block " ^ name ^ " */\n") ^
                        self#input_struct ^
                        self#output_struct ^
                        (* Initialize static constants and parameters *)
                        self#print_static ^
                        (* Function definition *)
                        (let out_struct = self#output_type in 
                          if out_struct <> ""
                          then out_struct
                          else "void") ^ 
                        "\n/* Function def */ " ^ name ^ "(" ^ 
                        (let in_struct = self#input_type in
                        if in_struct <> "" 
                        then in_struct ^ " inputs"
                        else "") ^ 
                        ")\n{\n" ^ 
                        (* Unpack inputs *)
                        (let input_blk = String.concat "\n\t" 
                            (List.map 
                            (fun x -> (get_datatype x.datatype) ^ " " ^ 
                                x.name ^ " = inputs." ^ x.name ^ ";"
                            ) 
                            self#inputs) in
                        if input_blk <> ""
                        then "\t/* Inputs for block " ^ name ^
                             " */\n\t" ^ input_blk ^ "\n\n"
                        else "") ^
                        (* Code for inner objects in SSA form *)
                        if_elements
                            self#inner_objs
                            ("\t/* Body for block " ^ name ^ " */\n\t" ^
                            (String.concat "\n\t" 
                                (List.map
                                    (fun x -> (x :> base) #body)
                                    (* Skip parts block takes care of *)
                                    (List.filter
                                        (fun x -> let c = 
                                                    (x :> base) #print_class
                                                   in
                                                    not (   c = "input" 
                                                         || c = "dt"    
                                                         || c = "constant"
                                                        )
                                        )
                                        self#inner_objs
                                    )
                                )
                            ) ^ "\n\n")

    method trailer    = (* Pack up outputs *)
                         if_elements
                            self#outputs
                            ("\t/* Outputs for block " ^ name ^" */\n\t" ^
                            self#output_type ^ " outputs;\n\t" ^
                            (String.concat ";\n\t" 
                                (List.map 
                                (fun x -> "outputs." ^ x.name ^ " = " ^ x.name) 
                                self#outputs)
                            ) ^ ";\n\n" ^
                            (* terminate function *)
                            "\treturn outputs;") ^
                        "\n}\n"
end;;

(* Parse referenced file for the referenced block and return it for down below *)
let get_file xml_obj =
    let r = (get_attr "ref" xml_obj)
         in match r with
               Ref r -> if r.reftype = "FILE"
                        then r.refroot
                        else object_error "Ref object only supports " ^
                                          "file references"
             | _     -> object_error "Incorrect Type for filename"

(* Get the referenced block in the right file for the given reference object *)
let get_ref_blk xml_obj = 
    let rec get_inner_blk blk_list xml_obj = 
        match blk_list with
            [] -> xml_obj
          | hd :: tl -> begin
                        let new_xml_obj =
                                (List.filter
                                    (fun x -> string_of_value 
                                                (get_attr "name" x) = hd)
                                     (List.filter
                                        (fun x -> x.tagname <> "CONNECTION")
                                        (xml_obj :: xml_obj.inner_objs)
                                     )
                                )
                         in if (List.length new_xml_obj) <> 1
                            then object_error ("Did not find exactly one " ^
                                              "referenced block")
                            else get_inner_blk tl (List.hd new_xml_obj)
                        end
     in let file = get_file xml_obj 
     in let xml_obj = (Xparser.xml_tree Xscanner.token 
                                (Lexing.from_channel (open_in file) )
                          ) (* Have to parse referenced 
                             * file to get block *)
        and blk_list = 
            let r = (get_attr "ref" xml_obj)
                in match r with
                       Ref r -> r.reflist
                     | _     -> object_error "Incorrect Type for block ref"
         in get_inner_blk blk_list xml_obj

(* Reference class: REFERENCE tag, references a block in another file *)
class reference blockify xml_obj = object (self)
    inherit blk_or_ref blockify xml_obj
    method func = string_of_value (get_attr "name" (get_ref_blk xml_obj))
    val mutable inner_objs   = List.map 
                                 blockify 
                                 (List.filter
                                    (fun x -> x.tagname <> "CONNECTION")
                                    (get_ref_blk xml_obj).inner_objs
                                 )
    method set_inner_objs new_inner_objs = object_error
                            ("Should not try to set inner objects of " ^
                             self#print_class ^ " object: " ^ self#name ^ "")
    method print_class  = "reference"
    method header       = let vlfile = (get_file xml_obj)
                           in let cfile = (Str.global_replace 
                                            (Str.regexp "\\.vl") 
                                            ".c" 
                                            vlfile
                                          )
                           in "#include \"" ^ cfile ^"\""
    method trailer      = ""
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
    val datatype = string_of_value (get_attr "datatype" xml_obj)
    method datatype = datatype
    val mutable inputs   = [{ name = string_of_value 
                                            (get_attr "name" xml_obj); 
                          datatype = string_of_value 
                                            (get_attr "datatype" xml_obj)
                            }]
    method inputs = inputs
    method set_inputs new_inputs = inputs <- new_inputs
    method outputs  = [{ name = self#name; datatype = self#datatype }]
    method set_outputs a = object_error (
                                "Should not set outputs of " ^
                                self#print_class ^ " object")
    method print_obj    = "\"" ^ self#print_class ^ "\": { " ^
                          "\"name\":\"" ^ name ^ "\", " ^
                          "\"datatype\":\"" ^ datatype ^ "\", " ^
                          " }"
    method header     = ""
    method body       = ""
    method trailer    = ""
end;;

(* Input class: INPUT tag*)
class input xml_obj = object (self)
    inherit io_part xml_obj as super
    method inputs   = object_error "Should never access inputs of input obj"
    method set_inputs  a = object_error 
                                ("Should not set inputs of " ^
                                self#print_class ^ " object")
    method print_class  = "input"
end;;

(* Output class: OUTPUT tag *)
class output xml_obj = object (self)
    inherit io_part xml_obj as super
    method print_class  = "output"
    method body       = get_datatype (List.hd self#outputs).datatype ^ " " ^
                        self#name ^ " = " ^
                        (List.hd self#inputs).name ^ ";"
end;;

(* Constant class: CONSTANT tag*)
class constant xml_obj = object (self)
    inherit input xml_obj (* A constant acts like an input, except it has 
                           * a value and doesn't interact with block I/O *)
    val value    = string_of_value (get_attr "value"    xml_obj)
    method value = value
    method header     = (* overriden for block#header*)
                        "static " ^ (get_datatype self#datatype) ^ " " ^
                        self#name ^ " = " ^ value ^ ";"
    method print_class  = "constant"
    method print_obj    = "\"" ^ self#print_class ^ "\": { " ^
                          "\"name\":\"" ^ name ^ "\", " ^
                          "\"value\":\"" ^ value ^ "\", " ^
                          " }"
end;;

(* DT class: starts as ic, gets updated each pass as delta t in code exec *)
class dt xml_obj = object (self)
    inherit base xml_obj
    method inner_objs = object_error 
                            ("Should not try to access inner objects of " ^
                             self#print_class ^ " object: " ^ self#name ^ "")
    method set_inner_objs new_inner_objs = object_error
                            ("Should not try to set inner objects of " ^
                             self#print_class ^ " object: " ^ self#name ^ "")
    method inputs   = object_error "Should never access inputs of dt obj"
    method set_inputs  a = object_error 
                                ("Should not set inputs of " ^
                                self#print_class ^ " object")
    method outputs  = [{ name = self#name; datatype = "single" }]
    method set_outputs a = object_error (
                                "Should not set outputs of " ^
                                self#print_class ^ " object")
    method datatype = "single"
    val init_cond       = string_of_value (get_attr "ic" xml_obj)
    method header     = "static " ^ (get_datatype self#datatype) ^ " " ^
                        self#name ^ " = " ^ init_cond ^ ";"
    method body         = ""
    method trailer      = ""
    method print_class  = "dt"
    method print_obj    = "\"" ^ self#print_class ^ "\": { " ^
                          "\"name\":\"" ^ name ^ "\", " ^
                          "\"ic\":\"" ^ init_cond ^ "\", " ^
                          " }"
end;;

(* All other parts inherit from this one *)
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
    val init_cond       = string_of_value (get_attr "ic" xml_obj)
    val mutable inputs   = [{ name = "current"; datatype = "auto" }]
    val mutable outputs  = [{ name = "stored"; datatype = "auto" }]
    val datatype = string_of_value (get_attr "datatype" xml_obj)
    method datatype = datatype
    method init_cond = init_cond
    method print_class  = "memory"
    method print_obj    = "\"memory\": { " ^
                          "\"name\":\"" ^ name ^ "\", " ^
                          "\"init_cond\":" ^ init_cond ^ "\"" ^
                          " }"
    method header     = (* overriden for block#header*)
                        "static " ^ (get_datatype self#datatype) ^ " " ^
                        self#name ^ " = " ^ init_cond ^ ";"
    method body         = self#name ^ " = " ^ 
                          (List.hd inputs).name ^ ";"
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
    method body         = (get_datatype (List.hd outputs).datatype) ^ " " ^ 
                          self#name ^ " = !(" ^ 
                          (List.hd inputs).name ^ ");"
end;;

(* Helper functions for binary operation parts, which can have an arbitrary
 * number of inputs, so long as there is at least 2. *)
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
              | _ -> let cnx_list = 
                            {name = idx_name; datatype = set_type} :: cnx_list
                    in create_cnx_list num_cnx cnx_list
     in create_cnx_list num_cnx []
    (* inputs for binop parts are named input1 through inputN 
    * and the operation will be applied on all elements *)

(* virtual Binary Operation class: do all binary attributes and checking *)
class virtual binop_part xml_obj = object (self)
    inherit part xml_obj
    val virtual operation : string
    method operation = operation
    method virtual datatype : string
    method print_obj    = "\"" ^ self#print_class ^ "\": { " ^
                          "\"name\":\"" ^ name ^ "\", " ^
                          "\"operation\":\"" ^ self#operation ^ "\" }"
    method body         = (get_datatype self#datatype) ^ " " ^ 
                          self#name ^ " = " ^ String.concat 
                                (" " ^ self#operation ^ " ")
                                (List.map 
                                    (fun x -> x.name) 
                                    self#inputs
                                ) ^ 
                          ";"
end;;

(* intermediate class to explicitly set datatype for gate parts *)
class virtual gate xml_obj = object
    inherit binop_part xml_obj as super
    val datatype = "boolean"
    method datatype = datatype
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

(* NOR gate: inherits from binary_gate_part, logical NOR operation *)
class nor_gate xml_obj = object (self)
    inherit gate xml_obj as super
    val operation = "" (* overriden body, operation is AND of NOT-ed inputs *)
    method print_class = "nor"
    method body         = (get_datatype self#datatype) ^ " " ^ 
                          self#name ^ " = !(" ^ String.concat 
                                (") && !(")
                                (List.map 
                                    (fun x -> x.name) 
                                    self#inputs
                                ) ^ 
                          ");"
end;;

(* NAND gate: inherits from binary_gate_part, logical NAND operation *)
class nand_gate xml_obj = object (self)
    inherit gate xml_obj
    val operation = "" (* overriden body, operation is OR of NOT-ed inputs *)
    method print_class = "nand"
    method body         = (get_datatype self#datatype) ^ " " ^ 
                          self#name ^ " = !(" ^ String.concat 
                                (") || !(")
                                (List.map 
                                    (fun x -> x.name) 
                                    self#inputs
                                ) ^ 
                          ");"
end;;

(* XOR gate: inherits from binary_gate_part, logical XOR operation *)
class xor_gate xml_obj = object (self)
    inherit gate xml_obj as super
    val operation = "" (* overriden body, operation is NEQ of each input *)
    method print_class = "xor"
    method body         = (get_datatype self#datatype) ^ " " ^ 
                          self#name ^ " = (" ^ String.concat 
                                (") != (")
                                (List.map 
                                    (fun x -> x.name) 
                                    self#inputs
                                ) ^ 
                          ");"
end;;

(* Summation point: inherits from binop_part, addition operation *)
class sum xml_obj = object (self)
    inherit binop_part xml_obj
    val operation = "+"
    val datatype = string_of_value (get_attr "datatype" xml_obj)
    method datatype = datatype
    method print_class = "sum"
    val mutable inputs  = get_cnx_list xml_obj "auto"
    val mutable outputs = [{ name = "output"; datatype = "auto" }]
end;;

(* Production point: inherits from binop_part, multiplication operation *)
class prod xml_obj = object (self)
    inherit binop_part xml_obj
    val operation = "*"
    val datatype = string_of_value (get_attr "datatype" xml_obj)
    method datatype = datatype
    method print_class = "prod"
    val mutable inputs  = get_cnx_list xml_obj "auto"
    val mutable outputs = [{ name = "output"; datatype = "auto" }]
end;;

(* GAIN Part class: unary multiplication operation *)
class gain xml_obj = object (self)
    inherit part xml_obj
    val mutable inputs  = [{ name = "input"; datatype = "auto" }]
    val mutable outputs = [{ name = "output"; datatype = "auto" }]
    val datatype = string_of_value (get_attr "datatype" xml_obj)
    method datatype = datatype
    val value    = string_of_value (get_attr "value"    xml_obj)
    method value = value
    method print_class  = "gain"
    method print_obj    = "\"" ^ self#print_class ^ "\": { " ^
                          "\"name\":\"" ^ name ^ "\", " ^
                          "\"datatype\":\"" ^ datatype ^ "\", " ^
                          "\"value\":\"" ^ value ^ "\" }"
    method body         = (get_datatype datatype) ^ " " ^ 
                          self#name ^ " = " ^ value ^ " * " ^
                          (List.hd inputs).name ^ ";"
end;;

(* INV Part class: unary inversion/division operation *)
class inv xml_obj = object (self)
    inherit part xml_obj
    val mutable inputs  = [{ name = "input"; datatype = "auto" }]
    val mutable outputs = [{ name = "output"; datatype = "auto" }]
    val datatype = string_of_value (get_attr "datatype" xml_obj)
    method datatype = datatype
    method print_class  = "inv"
    method print_obj    = "\"" ^ self#print_class ^ "\": { " ^
                          "\"name\":\"" ^ name ^ "\", " ^
                          "\"datatype\":\"" ^ datatype ^ "\" }"
    method body         = let input = (List.hd inputs).name in
                          (get_datatype datatype) ^ " " ^ 
                          self#name ^ " = " ^ 
                          (* Divide by zero protection *)
                          "(abs(" ^ input ^ ") >= FLT_MIN) ?\n\t\t" ^ 
                          "(1 / ( " ^ input ^ " )) : (0.000f);"
end;;

(* Compare Part: compares two inputs using operation *)
class compare xml_obj = object (self)
    inherit part xml_obj
    val operation = string_of_value (get_attr "operation" xml_obj)
    val datatype = string_of_value (get_attr "datatype" xml_obj)
    method datatype = datatype
    method print_class = "compare"
    val mutable inputs  = [{ name = "lhs"; datatype = "auto" }; 
                           { name = "rhs"; datatype = "auto" }]
    val mutable outputs = [{ name = "output"; datatype = "boolean" }]
    method body         = (get_datatype (List.hd outputs).datatype) ^ " " ^ 
                          self#name ^ " = (" ^ 
                            String.concat 
                                ( " " ^ operation ^ " ") 
                                (List.map (fun x -> x.name) self#inputs)
                          ^ ");"
    method print_obj    = "\"" ^ self#print_class ^ "\": { " ^
                          "\"name\":\"" ^ name ^ "\", " ^
                          "\"datatype\":\"" ^ datatype ^ "\", " ^
                          "\"operation\":\"" ^ operation ^ "\" }"
end;;

(* If part: if control is true, pass true input, else false input *)
class if_sw xml_obj = object (self)
    inherit part xml_obj
    val datatype = string_of_value (get_attr "datatype" xml_obj)
    method datatype = datatype
    method print_class = "if"
    val mutable inputs  = [{ name = "control"; datatype = "boolean" }; 
                           { name = "true"; datatype = "auto" };
                           { name = "false"; datatype = "auto" }]
    val mutable outputs = [{ name = "output"; datatype = "auto" }]
    method body         = (get_datatype datatype) ^ " " ^ 
                          self#name ^ " = (" ^ (List.nth self#inputs 0).name ^
                          ") ?\n\t\t(" ^ (List.nth self#inputs 1).name ^
                          ") :\n\t\t(" ^ (List.nth self#inputs 2).name ^
                          ");"
    method print_obj    = "\"" ^ self#print_class ^ "\": { " ^
                          "\"name\":\"" ^ name ^ "\", " ^
                          "\"datatype\":\"" ^ datatype ^ "\" }"
end;;

(* Main block management functions *)
(* Blockify goes through and matches the tagname to the appropiate object *)
let rec blockify xml_obj = 
    match xml_obj.tagname with
          "BLOCK"       -> (new block blockify xml_obj :> base)
        | "REFERENCE"   -> (new reference blockify xml_obj :> base)
          (* Note: passing blockify into block/ref instantiation because they
           * can't see at compile time what the function blockify refers to *)
        | "INPUT"       -> (new input       xml_obj :> base)
        | "OUTPUT"      -> (new output      xml_obj :> base)
        | "CONSTANT"    -> (new constant    xml_obj :> base)
        | "DT"          -> (new dt          xml_obj :> base)
        | "MEM"         -> (new memory      xml_obj :> base)
        | "NOT"         -> (new not_gate    xml_obj :> base)
        | "AND"         -> (new and_gate    xml_obj :> base)
        | "OR"          -> (new or_gate     xml_obj :> base)
        | "NAND"        -> (new nand_gate   xml_obj :> base)
        | "NOR"         -> (new nor_gate    xml_obj :> base)
        | "XOR"         -> (new xor_gate    xml_obj :> base)
        | "SUM"         -> (new sum         xml_obj :> base)
        | "PROD"        -> (new prod        xml_obj :> base)
        | "GAIN"        -> (new gain        xml_obj :> base)
        | "INV"         -> (new inv         xml_obj :> base)
        | "COMPARE"     -> (new compare     xml_obj :> base)
        | "IF"          -> (new if_sw       xml_obj :> base)
        (* CONNECTION blocks are not supported by this operation. 
         * See get_connection above *)
        | _ as name -> object_error ("Tag " ^ name ^ " not supported.")

(* Main caller function simply to protect against top level blocks not being
 * of type BLOCK *)
let parse_xml_tree xml_obj = 
    match xml_obj.tagname with
          "BLOCK"   -> blockify xml_obj
        | _ as name -> object_error
                            ("Tag " ^ name ^ " cannot be top level block")
