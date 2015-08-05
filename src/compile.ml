open Blockify
open Blockparse

let translate program =
    (* Print standard libraries required *)
      "#include <stdbool.h>\n"
    ^ "#include <stdint.h>\n"
    ^ "#include <float.h>\n"
    ^ "#include <math.h>\n"
    ^ "\n"
    (* Print print the code for each block in the program using the optimized and
     * ordered inner blocks in the body code method for each *)
    ^ String.concat "\n\n" (List.map 
                                (fun x -> let obj = (x :> base) in 
                                    obj#header ^ obj#body ^ obj#trailer
                                ) 
                                program
                           )
    ^ "\n/*Generated using VLCC */\n"
(* Generate python script for processing in files and sending it through the
 * compiled binary and printing the results as it is running *)
let gen_debug_code program =
    let top = ((List.hd (List.rev program)) :> base) in
        let name = top#name in
        let inputs = top#inputs 
         in
            "import sys\n"
          ^ "import ctypes\n"
          ^ "from ctypes import *\n"
          ^ "lib = cdll.LoadLibrary('./test-" ^ name ^ ".so')\n"
          ^ "class " ^ name ^ "_inputs(Structure):\n"
          ^ "    _fields_ = [(\""
          ^     (String.concat 
                    "), (\"" 
                    (List.map 
                        (fun x -> x.name ^ "\", "
                         ^  match x.datatype with
                                "uint8"  -> "c_uint8"
                              | "uint16" -> "c_uint16"
                              | "uint32" -> "c_uint32"
                              | "int8"   -> "c_int8"
                              | "int16"  -> "c_int16"
                              | "int32"  -> "c_int32"
                              | "single" -> "c_float"
                              | "double" -> "c_double"
                              | "boolean"-> "c_byte" (* Assume uint8 *)
                              | _ -> failwith "unassigned value"
                        )
                        inputs
                    )
                ) 
                ^ ")]\n"
          ^ "    \n"
          ^ "with open(sys.argv[1]) as f:\n"
          ^ "    for line in f:\n"
          ^ "        listargs = line.strip('\\n').split(',')\n"
          ^ "        inputs = " ^ name ^ "_inputs("
          ^     (String.concat
                    ", "
                    (List.mapi
                        (fun i x -> (
                            match x.datatype with 
                                "uint8"  -> "int"
                              | "uint16" -> "int"
                              | "uint32" -> "int"
                              | "int8"   -> "int"
                              | "int16"  -> "int"
                              | "int32"  -> "int"
                              | "single" -> "float"
                              | "double" -> "float"
                              | "boolean"-> "int" (* Assume uint8 *)
                              | _ -> failwith "unassigned value"
                            )
                            ^ "(listargs[" ^ string_of_int(i) ^ "])"
                        )
                        inputs
                    )
                ) 
                ^ ")\n"
          ^ "        outputs = lib." ^ name ^ "(inputs)\n"
          ^ "        print(outputs)"
