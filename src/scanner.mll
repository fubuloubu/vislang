{
    (* open Parser *)
    open Printf

    (* Define errors *)
    let error msg start finish  = 
            Printf.sprintf "(line %d: char %d..%d): %s" 
                    (start.Lexing.pos_lnum)
                    (start.Lexing.pos_cnum - start.Lexing.pos_bol) 
                    (finish.Lexing.pos_cnum - finish.Lexing.pos_bol)
                    msg
    exception XML_Error of string
    let xml_error lexbuf = raise
                    (XML_Error
                        (error 
                            ("Badly Formatted XML")
                            (lexbuf.Lexing.lex_start_p) 
                            (lexbuf.Lexing.lex_curr_p)
                        )
                    )
}
(* Main definitions for use below *)
let ws      = [' ' '\t']
let nl      = ['\r' '\n']
let name    = ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let file    = ("../" | "./" | "/")
              (['A'-'Z' 'a'-'z' '0'-'9' '_' '-' '.']+ ("/")?)+
              (".vl")
let cnx     = ("|" name)+
let sign    = ("+" | "-")
let boolean = ("true" | "false")
let digit   = ['0'-'9']
let flt_pt  = sign? (digit+ "." digit* | "." digit+)
let hexdig  = ['A'-'F' 'a'-'f' '0'-'9']
let hex     = sign? "0x" hexdig+ (* Allow signed hex numbers *)
let octdig  = ['0'-'7']
let octal   = "8x" octdig+
let bindig  = ['0' '1']
let binary  = "2x" bindig+
let decimal = sign? digit+ (* Allow signed decimals *)
let literal = (boolean | flt_pt | hex | decimal | binary | octal)
let op      = ("==" | ">" | "<" | ">=" | "<=" | "!=")
let attribute = (name '=' '"' ((name | file) cnx* | literal | op) '"')

(* Main scanner step: search for blocks and comments *)
rule token =
    parse ("<?" | "<!--") as ctype  { comment ctype lexbuf }
        | '<' (name as tag)         { block tag lexbuf }
        | "</" (name as tag) ">"    { printf 
                                        "Closing tag found for %s\n" tag; 
                                      token lexbuf }
        | ws                        { token lexbuf }
        | nl                        { Lexing.new_line lexbuf; token lexbuf }
        | _                         { xml_error lexbuf }
        | eof                       { exit 0 }
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
        | nl    { Lexing.new_line lexbuf; comment ctype lexbuf }
        | _     { comment ctype lexbuf }
(* Block sub-rule: Scan for supported blocks and link
 * to parsing stage. If an unsupported block is found, note
 * it as information for compilation *)
and block tag =
    parse ((ws | nl)+ attribute)* as attributes ws? ("/>" | ">") as blk_end
        {
            printf "Block %s contains: %s\n" tag attributes;
            (* Only the BLOCK tag is allowed to contain tags
             * underneath.*)
            if blk_end = ">"
            then printf "Block %s not closed\n" tag;
            token lexbuf
        }
        (* Note: attributes are only accepted in-tag e.g.:
         * <attr>inner attributes are ignored</attr> *)
        | ("/>" | ">" [^'\n' '\r']* "</" name as tag_to_chk ">") 
            {
                if tag_to_chk = tag
                then (printf "Ignoring Attribute %s\n" tag;
                    token lexbuf)
                else xml_error lexbuf
            }
        | _ { xml_error lexbuf }
{
    (* Code for test purposes *)
    let rec parse lexbuf =
        let () = token lexbuf in
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

    let _ = (*Printexc.print*) main ()
}
