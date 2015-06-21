{
   (* open Parser *)
    open Printf
}

(* Main definitions for use below *)
let digit = ['0'-'9']
let name  = ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let file  = (".." | '.')?
            (['/']['/' 'A'-'Z' 'a'-'z' '0'-'9' '_']*)*
            (".vl")

(* Main scanner step: search for blocks and comments *)
rule token =
    parse [' ' '\t' '\r' '\n']      { token lexbuf }
        | "<?"
        | "<!--" as comment_type    { comment comment_type lexbuf }
        | '<'                       { block lexbuf }
        | eof                       { EOF }
(* Comment sub-rule: search for matching comment tag.
 * If a different comment tag type found, then continue,
 * else return to main scanner.*)
and rule comment ctype =
    parse "-->" { if ctype = "<!--"
                  then token lexbuf
                  else comment ctype lexbuf }
        | "?>"  { if ctype = "<?"
                  then token lexbuf
                  else comment ctype lexbuf }
        | _     { comment ctype lexbuf }
        | eof   { raise "Reached end of file before we " ^
                        "saw a closing tag for comment\n" }
(* Block sub-rule: Scan for supported blocks and link
 * to parsing stage. If an unsupported block is found, note
 * it as information *)
and rule block =
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
        | name as tag   { printf "Info: tag %s is unsupported."
                                 "\nContinuing...\n" tag
                          generic_closure tag lexbuf }
        | eof           { raise "Reached end of file before " ^
                                "finishing block closure\n" }

(* Attribute sub-rules: Once supported blocks are found,
 * run the specific scanner rule for that block *)
