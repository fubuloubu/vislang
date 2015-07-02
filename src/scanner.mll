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
let dtype   = ("double" | "single" | "boolean" 
                | 'u'? "int" ("8" | "16" | "32"))
let scope   = ("global" | "local")
let file    = ("../" | "./" | "/")
              (['A'-'Z' 'a'-'z' '0'-'9' '_' '-' '.']+ ("/")?)+
              (".vl")
let sign    = ("+" | "-")
let boolean = ("true" | "false")
let digit   = ['0'-'9']
let flt_pt  = sign? (digit+ "." digit* | "." digit+)
let hex     = sign? ("0x" | "0X") ['A'-'F' 'a'-'f' '0'-'9']+
let oct     = sign? ("0o" | "0O") ['0'-'7']+
let bin     = sign? ("0b" | "0B") ['0' '1']+
let dec     = sign? digit+ (* Allow signed integers for any encoding *)
let op      = ("==" | ">" | "<" | ">=" | "<=" | "!=")

(* Main scanner step: search for blocks and comments *)
rule token =
    parse ("<?" | "<!--") as ctype  { comment ctype lexbuf }
        | '<' (name as tag)         { printf (* Replace with OTAG *)
                                        "Opening tag found for %s\n" tag;
                                      block tag lexbuf }
        | "</" (name as tag) ">"    { printf (* Replace with CTAG *)
                                        "Closing tag found for %s\n" tag; 
                                      token lexbuf }
        | ws                        { token lexbuf }
        | nl                        { Lexing.new_line lexbuf; token lexbuf }
        (* This is here to allow anything between attribute tags to work *)
        | _ as c                    { printf "Warning, undefined XML: %c\n" c;
        (* TODO: Find a better way *) token lexbuf }
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
    parse ws                { block tag lexbuf }
        | nl    { Lexing.new_line lexbuf; block tag lexbuf }
        (* Note: attributes are only accepted in-tag e.g.:
         * <attr>inner attributes are ignored</attr> *)
        | name as n "=\""   { printf "Attribute %s: " n; value tag lexbuf }
        | "/>"              { printf (* Replace with CTAG *)
                                     "Closing tag found for %s\n" tag;
                              token lexbuf }
        | ">"               { printf "Warning: Unclosed block %s\n" tag;
                              token lexbuf }
        | _                 { xml_error lexbuf }
and value tag =
    parse ws                { value tag lexbuf }
        | nl                { Lexing.new_line lexbuf; value tag lexbuf }
        | dtype as d        { printf "%s (Datatype) " d; value tag lexbuf }
        | scope as s        { printf "%s (Scope) " s; value tag lexbuf }
        | name as n         { printf "%s (Name) " n; value tag lexbuf }
        | file as f         { printf "%s (File) " f; value tag lexbuf }
        | "|" (name as r)   { printf "%s (Ref) " r; value tag lexbuf }
        | "\""              { printf "\n"; block tag lexbuf }
        (* Only allow recursive calls for the above types by restricting
         * to terminated values *)
        | op as o "\""      { printf "%s (Operator)\n" o; block tag lexbuf }
        | boolean as b "\"" { printf "%s (Bool)\n" b; block tag lexbuf }
        | flt_pt as f "\""  { printf "%s (Float)\n" f; block tag lexbuf }
        (* Integer types *)
        | hex as h "\""     { printf "%s (Hex)\n" h; block tag lexbuf }
        | dec as d "\""     { printf "%s (Dec)\n" d; block tag lexbuf }
        | oct as o "\""     { printf "%s (Oct)\n" o; block tag lexbuf }
        | bin as b "\""     { printf "%s (Bin)\n" b; block tag lexbuf }
        | _                 { xml_error lexbuf }
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
