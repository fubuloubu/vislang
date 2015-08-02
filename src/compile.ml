open Blockify
open Blockparse

let translate program =
    (* Print standard libraries required *)
      "#include <stdint.h>\n"
    ^ "#include <float.h>\n"
    ^ "#include <math.h>\n\n"
    (* Print header for all the objects in the ordered list, then trailers *)
    ^ String.concat "\n" (List.map (fun x -> (x :> base) #header ) program)
    ^ String.concat "\n" (List.map (fun x -> (x :> base) #trailer) program)

(* Generate python script for processing in files and sending it through the
 * compiled binary and printing the results as it is running *)
let gen_debug_code top =
    let top = (top :> base) in
        let name = top#name in
        let inputs = top#get_inputs 
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
          ^ "        inputs = buffer_inputs("
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
