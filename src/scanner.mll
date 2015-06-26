{
   (* open Parser *)
    open Printf

    (* current token line number *)
    let line_num = ref 1

    (* Define errors *)
    (*let error msg start finish  = 
            Printf.sprintf "(line %d: char %d..%d): %s" 
                    Position.pos_lnum start
                    (Position.pos_cnum start - Position.pos_bol start) 
                    (Position.pos_cnum finish - Position.pos_bol finish)
                    msg
   *) 
    exception XML_Error of string
    let xml_error lexbuf = 
            raise ( XML_Error "Bad XML BAD!" 
                    (* error "Bad XML"
                    (Lexbuf.lex_start_p lexbuf) 
                    (Lexbuf.lex_curr_p lexbuf) *)
                  )
}
(* Main definitions for use below *)
let ws = [' ' '\t' '\r' '\n']
(*TODO: I think name and attribute being incorrect is causing the issue *)
let name  = ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let file  = ("../" | "./" | "/")
            (['A'-'Z' 'a'-'z' '0'-'9' '_' '.']+ ("/")?)+
            (".vl")
let cnx = ("|" name)+

let sign = ("+" | "-")
let digit = ['0'-'9']
let flt_pt = sign? (digit+ "." digit* | "." digit+)
let intpfx = ("0x" | "2x" | "8x" | sign)
let op = ("==" | ">" | "<" | ">=" | "<=" | "!=")
let attribute = (name '=' '"'
                (name cnx* | flt_pt | intpfx? digit+ | op) '"')

(* Main scanner step: search for blocks and comments *)
rule token =
    parse ("<?" | "<!--") as ctype  { comment ctype lexbuf }
        | '<' (name as tag)         { block tag lexbuf }
        | "</" (name as tag) ">"    { printf 
                                        "Closing tag found for %s\n" tag; 
                                      token lexbuf }
        | ws                        { token lexbuf }
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
        | _     { comment ctype lexbuf }
(* Block sub-rule: Scan for supported blocks and link
 * to parsing stage. If an unsupported block is found, note
 * it as information for compilation *)
and block tag =
    parse (ws+ attribute)* as attributes ws? ("/>" | ">") as blk_end
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
