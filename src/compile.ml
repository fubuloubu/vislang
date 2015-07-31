open Blockify
open Bytecode

let translate program =
      String.concat "\n" (List.map (fun x -> (x :> base) #header  ) program)
    ^ String.concat "\n" (List.map (fun x -> (x :> base) #bytecode) program)
    ^ String.concat "\n" (List.map (fun x -> (x :> base) #trailer ) program)
