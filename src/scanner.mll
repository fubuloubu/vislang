{
   (* open Parser *)
    open Printf
}

(* Main definitions for use below *)
let digit = ['0'-'9']
let name  = ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let file  = ("../" | "./" | "/")
            (['A'-'Z' 'a'-'z' '0'-'9' '_' '.']+ ("/")?)+
            (".vl")
let cnx = ("|" name)+
let flt_pt = ("+" | "-")? (digit+ "." digit* | "." digit+)
let intpfx = ("0x" | "2x" | "8x" | "-" | "+")?

let attribute = (' ' name '=' "\"" (name cnx? | flt_pt | intpfx digit+)+ "\"")

(* Main scanner step: search for blocks and comments *)
rule token =
    parse [' ' '\t' '\r' '\n']      { token lexbuf }
        | "<?"
        | "<!--" as comment_type    { comment comment_type lexbuf }
        | '<'                       { block lexbuf }
        | eof                       { printf "Reached end of file" }
(* Comment sub-rule: search for matching comment tag.
 * If a different comment tag type found, then continue,
 * else return to main scanner.*)
and comment ctype =
    parse "-->" { if ctype = "<!--"
                  then token lexbuf
                  else comment ctype lexbuf }
        | "?>"  { if ctype = "<?"
                  then token lexbuf
                  else comment ctype lexbuf }
        | _     { comment ctype lexbuf }
        | eof   { token lexbuf }
(* Block sub-rule: Scan for supported blocks and link
 * to parsing stage. If an unsupported block is found, note
 * it as information *)
and block =
    (* Block constructs *)
    parse "PROGRAM"      as tag { generic_closure tag lexbuf }
        | "BLOCK"        as tag { generic_closure tag lexbuf }
        | "CONNECTION"   as tag { generic_closure tag lexbuf }
    (* Name constructs *)
        | "INPUT"        as tag { generic_closure tag lexbuf }
        | "OUTPUT"       as tag { generic_closure tag lexbuf }
        | "CONSTANT"     as tag { generic_closure tag lexbuf }
        | "SIGNAL"       as tag { generic_closure tag lexbuf }
    (* Atomic Parts *)
        | "CAST"         as tag { generic_closure tag lexbuf }
        | "MEM"          as tag { generic_closure tag lexbuf }
        | "DT"           as tag { generic_closure tag lexbuf }
        | "NOT"          as tag { generic_closure tag lexbuf }
        | "AND"          as tag { generic_closure tag lexbuf }
        | "OR"           as tag { generic_closure tag lexbuf }
        | "NOR"          as tag { generic_closure tag lexbuf }
        | "NAND"         as tag { generic_closure tag lexbuf }
        | "XOR"          as tag { generic_closure tag lexbuf }
        | "BITWISE"      as tag { generic_closure tag lexbuf }
        | "IF"           as tag { generic_closure tag lexbuf }
        | "COMPARE"      as tag { generic_closure tag lexbuf }
        | "SUM"          as tag { generic_closure tag lexbuf }
        | "PROD"         as tag { generic_closure tag lexbuf }
        | "GAIN"         as tag { generic_closure tag lexbuf }
        | "INV"          as tag { generic_closure tag lexbuf }
    (* Structures and Arrays *)
        | "MUX"          as tag { generic_closure tag lexbuf }
        | "DEMUX"        as tag { generic_closure tag lexbuf }
        | "STRUCT"       as tag { generic_closure tag lexbuf }
        | "CONSTRUCT"    as tag { generic_closure tag lexbuf }
        | "DESTRUCT"     as tag { generic_closure tag lexbuf }
    (* Functional Operations *)
        | "MAP"          as tag { generic_closure tag lexbuf }
        | "FILTER"       as tag { generic_closure tag lexbuf }
        | "REDUCE"       as tag { generic_closure tag lexbuf }
    (* Simulation Parts *)
        | "SIGGEN"       as tag { generic_closure tag lexbuf }
        | "SCOPE"        as tag { generic_closure tag lexbuf }
    (* If unmatched, run generic tag closure scanner *) 
        | name as tag attribute* as attributes ">"
                        { generic_closure tag lexbuf }
        | name as tag attribute* as attributes "/>" 
                        { token lexbuf }
        | eof           { token lexbuf }
and generic_closure tag =
    parse "</" name as check_tag ">" { if tag = check_tag
                                       then token lexbuf
                                       else generic_closure tag lexbuf }
        | eof                   { token lexbuf }
(* Attribute sub-rules: Once supported blocks are found,
 * run the specific scanner rule for that block *)
{
    let rec parse lexbuf =
        let () = token lexbuf in
        (* do nothing in this example *)
        parse lexbuf

    let main () =
        let cin =
            if Array.length Sys.argv > 1
            then open_in Sys.argv.(1)
            else stdin
        in
        let lexbuf = Lexing.from_channel cin in
        try parse lexbuf
        with End_of_file -> ()

    let _ = Printexc.print main ()
}
