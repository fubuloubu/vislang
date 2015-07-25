open Xst
open Bot
open Errors

let get_attr attribute xml_obj = 
    let attr = List.filter (fun x -> x.aname = attribute) xml_obj.attributes in
        match attr with
            []      -> object_error ("No attribute named " ^ attribute )
          | [a]     -> a.avalue
          | _ :: _  -> object_error ("Too many attributes named " ^ attribute)

let blockify xml_obj = 
    match xml_obj.tagname with
          "BLOCK"   -> new block xml_obj
        (*| "INPUT"   -> new input    xml_obj
        | "OUTPUT"  -> new output   xml_obj
        *)(* CONNECTION blocks are not supported by this operation *)
        | _ as name     -> object_error ("Tag " ^ name ^ " not supported.")
