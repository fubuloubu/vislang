open Blockify
(* Block Parse intelligently traces through the objects inside a block from
 * output to input and finds an appropiate path through the program such
 * that when the bytecode is extracted in the order obtained here,
 * the program is consistent and no runtimes issues occur. *)
let rec block_parse top =
    (* TEMP: *) (top :> block) #inner_objs
    (*let outputs  = List.filter 
        (fun x -> x.tagname = "OUTPUT")
        xml_list :: List.filter 
        (fun x -> x.tagname = "MEM")
        xml_list in
    let rec reduce_xml_list xml_list obj_list =
        match obj_list with
            []       -> xml_list
          | hd :: tl -> let xml_list = List.filter 
                        (fun x -> (get_attr "name" x) <> (hd :> base) #name)
                        xml_list in
                        reduce_xml_list xml_list obj_list in
    (* Algorithm: 
     * The block trace algorithm will get the list of outputs from the
     * current block level, and recursively traverse the current object
     * list by finding the connection made from each input (starting at
     * the output), and tracing it back to it's last output. The recursion
     * will continue until either: an input is found (terminate that branch),
     * a memory block is found (terminate that branch, and add memory's input
     * to the list of traversals), a traversal is made to an object on the list
     * of prior traversals (see below), or an algebraic loop is detected (raise
     * error if the next traversal is already in the list of traversals made).
     * At the termination of a traversal for an output, all of the objects
     * detected are blockified and the entire list of objects is added to the
     * list of priors. This process continues until all output and memory
     * blocks successfully traverse back to inputs or priors branches. *)
    let rec trace prior_list trace_list xml_list curr =
        match xml_list with
            []       -> prior_list
          | hd :: tl -> let curr = (blockify hd)
                        and prior_list = prior_list :: curr in
                        trace prior_list trace_list tl curr 
        (*let connections = (curr :> base) #get_inputs
        (* filter xml_list to obtain all blocks referenced by connections *)
        let connected_blocks = *)
        (* if numbers don't match, something went wrong *)
        (* do a match on block type *)
        (* if block is input, blockify and terminate*)
        (* if block is mem, terminate*)
        (* if block is in current trace_list, raise error*)
        (* if block is in prior_list, terminate *)
        (* else, blockify block and add it to trace_list
         * then recurse on that block*)
    (* return object trace to block_trace *)
    in let rec output_trace outputs xml_list obj_list =
        match outputs with 
            []       -> obj_list
          | hd :: tl -> let obj_list = trace obj_list [] xml_list (blockify hd) 
                        and xml_list = reduce_xml_list xml_list obj_list
                        in output_trace tl xml_list obj_list

    in output_trace outputs xml_list []*)

let parse_block_tree program = 
    String.concat "\n" (
        List.map (fun x -> (x :> base) #bytecode) 
        (block_parse program)
    )
