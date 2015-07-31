open Xst
open Errors
(* Helper functions for Object instantiaion *)
let get_attr attribute xml_obj = 
    let attr = List.filter (fun x -> x.aname = attribute) xml_obj.attributes in
        match attr with
            []      -> object_error ("No attribute named " ^ attribute )
          | [a]     -> a.avalue
          | _ :: _  -> object_error ("Too many attributes named " ^ attribute)

let get_num_connection xml_obj =
    let inputs = List.filter
                 (fun x -> x.tagname = "CONNECTION")
                 xml_obj.inner_objs in
        List.length inputs

let get_connection input_to xml_obj =
    let input_from = List.filter (fun x -> (get_attr "to" x) = input_to)
        (List.filter (fun x -> x.tagname = "CONNECTION") xml_obj.inner_objs) in
        match input_from with
            []      -> object_error ("No connection found for " )
          | [from]  -> from
          | _ :: _  -> object_error ("Too many connections defined for " )

(* virtual Base class all blocks inherit from *)
class virtual base parent xml_obj = object
    val name : string   = Xst.string_of_value (get_attr "name" xml_obj)
    method name         = name
    val parent : string = parent
    method parent       = parent
    method virtual get_inputs   : string list
    method virtual get_outputs  : string list
    method virtual inner_objs   : base list
    method virtual print_class  : string
    method virtual header       : string
    method virtual bytecode     : string
    method virtual trailer      : string
    method virtual interface    : string
    method virtual print_obj    : string
end;;

(* Block class: BLOCK tag
 * inherits from base, is a container for other blocks *)
class block blockify parent xml_obj = object (self)
    inherit base parent xml_obj as super
    val inner_objs = 
        List.map 
            (blockify (Xst.string_of_value (get_attr "name" xml_obj))) 
            (xml_obj.inner_objs)
    method inner_objs = inner_objs
    method get_inputs =
        List.map    (fun (x : base) -> (x :> base) #name)
        (List.filter (fun (x : base) -> ((x :> base) #print_class) = "input")
                    inner_objs)
    method get_outputs =
        List.map    (fun (x : base) -> (x :> base) #name)
        (List.filter (fun (x : base) -> ((x :> base) #print_class) = "output")
                    inner_objs)
    method print_class= "block"
    method header     = "block: " ^ name ^ "\n" ^
                        name ^ ".inputs = { " ^
                        (String.concat "; " self#get_inputs) ^
                        " }\n" ^
                        name ^ ".outputs = { " ^
                        (String.concat "; " self#get_outputs) ^
                        " }\n\n"
    method bytecode   = ""
    method trailer    = ""
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
class virtual io_part parent xml_obj = object (self)
    inherit base parent xml_obj as super
    val scope    = Xst.string_of_value (get_attr "scope"    xml_obj)
    val datatype = Xst.string_of_value (get_attr "datatype" xml_obj)
    val size     =                      get_attr "size"     xml_obj
    method bytecode  =  "/* No bytecode for " ^ self#print_class ^ " " ^ name ^ "*/\n"
    method inner_objs   = object_error ("Should never try and access " ^
                                       "inner objects of " ^ self#print_class)
    method print_obj    = "\"" ^ self#print_class ^ "\": { " ^
                          "\"name\":\"" ^ name ^ "\", " ^
                          "\"scope\":" ^ scope ^ "\", " ^
                          "\"size\":" ^ Xst.string_of_value (size) ^ "\" }"
end;;

(* Input class: INPUT tag*)
class input parent xml_obj = object (self)
    inherit io_part parent xml_obj as super
    method get_inputs   = object_error "Should never access inputs of input obj"
    method get_outputs  = [(*FIXME: get_connection name xml_obj*)]
    method print_class  = "input"
    method header     = name ^ " = " ^ parent ^ ".inputs." ^ name ^ "\n"
    method trailer    = ""
    method interface  = ""
end;;

(* Output class: OUTPUT tag *)
class output parent xml_obj = object (self)
    inherit io_part parent xml_obj as super
    method get_inputs   = [(*FIXME: get_connection name xml_obj*)]
    method get_outputs  = object_error "Should never access outputs of output obj"
    method print_class = "output"
    method header     = ""
    method trailer    = ""
    method interface  = ""
end;;

(* Memory class: MEM tag*)
class memory parent xml_obj = object (self)
    inherit base parent xml_obj as super
    val init_cond       = get_attr "ic" xml_obj
    method inner_objs   = object_error ("Should never try and access " ^
                                       "inner objects of " ^ self#print_class)
    method get_inputs   = [(name ^ "_current")]
    method get_outputs  = [(name ^ "_stored")]
    method print_class  = "memory"
    method header     = ""
    method bytecode     = "bytecode for memory " ^ name ^ "\n"
    method trailer    = ""
    method interface  = ""
    method print_obj    = "\"memory\": { " ^
                          "\"name\":\"" ^ name ^ "\", " ^
                          "\"init_cond\":" ^ 
                          Xst.string_of_value (init_cond) ^ "\" }"
end;;

(* Main block management functions *)
(* Blockify goes through and matches the tagname to the appropiate object *)
let rec blockify parent xml_obj = 
    match xml_obj.tagname with
          "BLOCK"   -> (new block blockify parent xml_obj :> base)
          (* Note: passing blockify into block instantiation because it can't 
           * see at compile time what the function blockify is referring to *)
        | "INPUT"   -> (new input  parent xml_obj :> base)
        | "OUTPUT"  -> (new output parent xml_obj :> base)
        (* CONNECTION blocks are not supported by this operation. 
         * See get_connection above *)
        | _ as name -> object_error ("Tag " ^ name ^ " not supported.")

(* Main caller function simply to protect against top level blocks not being
 * of type BLOCK *)
let parse_xml_tree xml_obj = 
    match xml_obj.tagname with
          "BLOCK"   -> blockify "" xml_obj
        | _ as name -> object_error ("Tag " ^ name ^ " cannot be top level block")
