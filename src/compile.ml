open Blockify
open Blockparse

let translate program =
      String.concat "\n" (List.map (fun x -> (x :> base) #header ) program)
    ^ String.concat "\n" (List.map (fun x -> (x :> base) #trailer) program)

let gen_debug_code top =
    let top = (top :> base) in
        let name = top#name in
        let inputs = List.map 
            (fun x -> {name = x.name; datatype = "c_" ^ x.datatype})
            top#get_inputs 
         in
            "import sys\n" ^
            "from ctypes import *\n" ^
            "lib = cdll.LoadLibrary('./test-" ^ name ^ ".so')\n" ^
            "class " ^ name ^ "_inputs(Structure):\n" ^
            "    _fields_ = [(\"" ^ 
                (String.concat 
                    "), (\"" 
                    (List.map 
                        (fun x -> x.name ^ "\"" ^ x.datatype)
                        inputs
                    )
                ) ^ 
                ")]\n" ^
            "    \n" ^
            "with open(sys.argv[1]) as f:\n" ^
            "    for line in f:\n" ^
            "        listargs = line.strip('\n').split(',')\n" ^
            "        inputs = buffer_inputs(" ^ 
                (String.concat
                    "]), "
                    (List.mapi
                        (fun i x -> x.datatype ^ "(listargs[" ^ string_of_int(i) )
                        inputs
                    )
                ) ^ "]))\n" ^
            "        outputs = lib." ^ name ^ "(inputs)\n" ^
            "        print(outputs)"
