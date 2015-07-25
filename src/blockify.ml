open Xst
open Errors

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
            []      -> object_error ("No connection found for " ^ input_to )
          | [from]  -> from
          | _ :: _  -> object_error ("Too many connections defined for " ^ input_to )

let blockify xml_obj = 
    match xml_obj.tagname with
          "BLOCK"   -> new block    xml_obj
        | "INPUT"   -> new input    xml_obj
        | "OUTPUT"  -> new output   xml_obj
        (* CONNECTION blocks are not supported by this operation. 
         * See get_connection above *)
        | _ as name     -> object_error ("Tag " ^ name ^ " not supported.")
