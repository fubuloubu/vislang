open Errors
open Xst
open Blockify

(* virtual Base class all blocks inherit from *)
class virtual base xml_obj = object
    val name = get_attr "name" xml_obj
    method virtual self_check   : bool
    method virtual get_inputs   : string list
    method virtual init_code    : string
    method virtual body_code    : string
    method virtual final_code   : string
end;;

(* Block class: inherits from base, is a container for other blocks *)
class block xml_obj = object (self)
    inherit base xml_obj as super
    val inner_objs = List.map blockify xml_obj.inner_objs
    method self_check = false
    method get_inputs = []
    (*    List.map (fun x -> x#name)
        List.filter (fun x -> class(x) = "input") inner_objs
   *) method init_code  = ""
    method body_code  = ""
    method final_code = ""
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
end;;

(* Output class: *)
class output xml_obj = object (self)
    inherit io_part xml_obj as super
    method get_inputs = []
    method init_code  = ""
    method body_code  = ""
    method final_code = ""
end;;
