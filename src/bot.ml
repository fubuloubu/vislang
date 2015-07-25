open Errors
open Xst
open Blockify

(* Base class all blocks inherit from *)
class virtual base xml_obj = object
    method name = get_attr "name" xml_obj
    method virtual self_check   : bool
    method virtual get_inputs   : string list
    method virtual init_code    : string
    method virtual body_code    : string
    method virtual final_code   : string
end;;

class block xml_obj = object (self)
    inherit base xml_obj as super
    val inner_objs = List.map blockify xml_obj.inner_objs
    method self_check = false
    method get_inputs = []
    method init_code  = ""
    method body_code  = ""
    method final_code = ""
end;;
