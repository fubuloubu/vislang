open Blockify
open Errors
(* Block Parse intelligently traces through the objects inside a block from
 * output to input and finds an appropiate path through the program such
 * that when the bytecode is extracted in the order obtained here,
 * the program is consistent and no runtimes issues occur. *)
let rec block_parse top =
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
    let rec trace block_list prior_list trace_list current =
        match ((current :> base) #print_class) with
            "input"     -> current :: trace_list
          | "memory"    -> current :: trace_list
          | "const"     -> current :: trace_list
          | "dt"        -> current :: trace_list
          | _ as blk    -> (*FIXME: let () = Printf.printf "%s\n" blk in;*)
          let compare_obj n = (fun x -> (x :> base) #name = n)
           in
            if List.exists (compare_obj current#name) trace_list
            then object_error (blk ^ ": " ^ ((current :> base) #name) ^
                                     " is in an algebraic loop...")
            else if List.exists (compare_obj current#name) prior_list
                 then current :: trace_list
                 else let input_names = (List.map 
                                        (fun x -> x.name) 
                                        (current :> base) #get_inputs) 
                      and find_fun = (fun x -> List.find (compare_obj x) block_list)
                       in let input_list = (List.map find_fun input_names)
                       in trace_split block_list prior_list trace_list input_list
    (* for each input of a block, trace out the list from that point on *)
    and trace_split block_list prior_list trace_list input_list =
        match input_list with
            []          -> trace_list
          | hd :: tl    -> let trace_list = trace_list @
                               (trace block_list prior_list trace_list hd)
                            in trace_split block_list prior_list trace_list tl
            
    (* trace_start function: this function is the wrapper used to call the
     * inner trace algorithm. It recurses through the list of start objects,
     * applying the trace algorithm for each object, then appending the result
     * to the list of priors for the next recursion *)
     in 
     let rec trace_start block_list prior_list start_list =
        match start_list with
            []       -> prior_list
          | hd :: tl -> let prior_list = prior_list @ 
                            (trace block_list prior_list [] hd)
                         in trace_start block_list prior_list tl

    (* start_list: the list of objects in the top block used to prime the trace 
     * algorithm. All outputs and memory blocks are added to the start list
     * because *)
     in 
    let objs = (top :> base) #inner_objs
     in 
    let start_list  = 
       (List.filter (fun x -> (x :> base) #print_class = "output") objs) 
     @ (List.filter (fun x -> (x :> base) #print_class = "memory") objs) 
    
    (* Kick off wrapper function *)
     in top :: (trace_start objs start_list [])

