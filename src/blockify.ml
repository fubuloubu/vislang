open Xst
open Errors
(* Helper functions for Object instantiaion *)
let get_attr attribute xml_obj = 
    let attr = List.filter (fun x -> x.aname = attribute) xml_obj.attributes in
        match attr with
            []      -> object_error ("No attribute named " ^ attribute )
          | [a]     -> a.avalue
          | _ :: _  -> object_error ("Too many attributes named " ^ attribute)

let get_connection input_to xml_obj =
    let input_from = List.filter (fun x -> (get_attr "to" x) = input_to)
        (List.filter (fun x -> x.tagname = "CONNECTION") xml_obj.inner_objs) in
        match input_from with
            []      -> object_error ("No connection found for " )
          | [from]  -> from
          | _ :: _  -> object_error ("Too many connections defined for " )

(* virtual Base class all blocks inherit from *)
class virtual base xml_obj = object
    val name = get_attr "name" xml_obj
    method virtual self_check   : bool
    method virtual get_inputs   : string list
    method virtual init_code    : string
    method virtual body_code    : string
    method virtual final_code   : string
    method virtual print_obj    : string
end;;

(* Block class: inherits from base, is a container for other blocks *)
class block block_trace xml_obj = object (self)
    inherit base xml_obj as super
    val inner_objs = block_trace xml_obj.inner_objs
    method self_check = false
    method get_inputs = []
    (*    List.map (fun x -> x#name)
        List.filter (fun x -> class(x) = "input") inner_objs
   *) method init_code  = ""
    method body_code  = ""
    method final_code = ""
    method print_obj = ""
end;;

(* virtual I/O Part class: do all I/O Part attributes and checking *)
class virtual io_part xml_obj = object (self)
    inherit base xml_obj as super
    method scope    = get_attr "scope"    xml_obj
    method datatype = get_attr "datatype" xml_obj
    method size     = get_attr "size"     xml_obj
    method self_check = true
end;;

(* Input class: *)
class input xml_obj = object (self)
    inherit io_part xml_obj as super
    method get_inputs = []
    method init_code  = ""
    method body_code  = ""
    method final_code = ""
    method print_obj = ""
end;;

(* Output class: *)
class output xml_obj = object (self)
    inherit io_part xml_obj as super
    method get_inputs = []
    method init_code  = ""
    method body_code  = ""
    method final_code = ""
    method print_obj = ""
end;;

(* Main block management functions *)
(* Blockify goes through and matches the tagname to the appropiate object *)
let rec blockify xml_obj = 
    match xml_obj.tagname with
          "BLOCK"   -> (new block block_trace xml_obj :> base)
          (* Note: passing blockify into block instantiation because it can't 
           * see at compile time what the function blockify is referring to *)
        | "INPUT"   -> (new input   xml_obj :> base)
        | "OUTPUT"  -> (new output  xml_obj :> base)
        (* CONNECTION blocks are not supported by this operation. 
         * See get_connection above *)
        | _ as name -> object_error ("Tag " ^ name ^ " not supported.")

(* Block Trace intelligently traces through the objects from output to input and
 * finds an appropiate path through the program such that when it is compiled
 * in the order determined to be okay by block trace, no issues occur *)
and block_trace obj_list =
(* TODO: This needs to be smarter and "trace" through inner blocks *)
    List.map blockify obj_list

(* Main caller function simply to protect against top level blocks not being
 * of type BLOCK *)
let parse_tree xml_obj = 
    match xml_obj.tagname with
          "BLOCK"   -> blockify xml_obj
        | _ as name -> object_error ("Tag " ^ name ^ " cannot be top level block")
