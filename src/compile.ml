open Blockify
open Blockparse

let translate program =
      String.concat "\n" (List.map (fun x -> (x :> base) #header ) program)
    ^ String.concat "\n" (List.map (fun x -> (x :> base) #body   ) program)
    ^ String.concat "\n" (List.map (fun x -> (x :> base) #trailer) program)
