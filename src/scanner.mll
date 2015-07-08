{
    (* open Parser *)
    open Printf

    (* Function for debugging *)
    let debug token value =
        printf "(%s): %s\n" token value

    (* Define errors *)
    let lex_issue msg start finish  = 
            Printf.sprintf "(line %d: char %d..%d): %s" 
                    (start.Lexing.pos_lnum)
                    (start.Lexing.pos_cnum - start.Lexing.pos_bol) 
                    (finish.Lexing.pos_cnum - finish.Lexing.pos_bol)
                    msg
    exception XML_Error of string
    let xml_error lexbuf = raise
                    (XML_Error
                        (lex_issue 
                            ("Badly Formatted XML")
                            (lexbuf.Lexing.lex_start_p) 
                            (lexbuf.Lexing.lex_curr_p)
                        )
                    )
    let xml_warning lexbuf =
                        (lex_issue 
                            ("Warning -- Skipping XML")
                            (lexbuf.Lexing.lex_start_p) 
                            (lexbuf.Lexing.lex_curr_p)
                        )
}
(* Main definitions for use below *)
let ws      = [' ' '\t']
let nl      = ['\r' '\n']
let blktag  = ['A'-'Z']+ (* All accepted tags are upper case. TODO: insert list of 
                                                             * accepted tags here *)
let attr    = ['a'-'z']+ (* All accepted attrs are lower case. TODO: insert list of 
                                                             * accepted attrs here *)
let name    = ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let dtype   = ("double" | "single" | "boolean" | 'u'? "int" ("8" | "16" | "32"))
let scope   = ("global" | "local")
let file    = (".." | ".")? ("/" ['A'-'Z' 'a'-'z' '0'-'9' '_' '-' '.']+ )+ ".vl"
let sign    = ("+" | "-")
let boolean = ("true" | "false")
let digit   = ['0'-'9']
let flt_pt  = sign? (digit+ "." digit* | "." digit+)
let hex     = sign? '0' ['x' 'X'] ['A'-'F' 'a'-'f' '0'-'9']+
let oct     = sign? '0' ['o' 'O'] ['0'-'7']+
let bin     = sign? '0' ['b' 'B'] ['0' '1']+
let dec     = sign? digit+ (* Allow signed integers for any encoding *)
let cmp_op  = ("==" | ">" | "<" | ">=" | "<=" | "!=")
let bw_op   = ("or" | "and" | "not" | "xor" | "nor" | "nand" | "xnor")

(* Main scanner step: search for elements, attributes, and comments *)
rule token =
    parse
    (* Comments: Search for any of the following ignored tag openings, 
     * then jump to rule for parsing an ignore anything inside it. *) 
        "<?"   | (* XML Declarator *)
        "<!--" | (* XML Comments *)
        "<!["    (* DOCTYPE Markup *)
        as ctype                    {                      comm ctype lexbuf }
    (* Elements: Scan for supported blocks and link to parsing stage. 
     * If an unsupported block is found, note it as information for compilation *)
        | '<' (blktag as b)         { debug "O_ELEM ELEM" b;    token lexbuf }
        | "</" (blktag as b) ">"    { debug "C_ELEM" "n/a";     token lexbuf }
        | "/>"                      { debug "C_ELEM" "n/a";     token lexbuf }
        | ">"                       { (* No tag required *)     token lexbuf }
    (* Attributes: The following are tokens for different values
     * attributes might take on. *)
        | attr as a "="             { debug "ATTR" a;           token lexbuf }
        | "\"" (dtype as d) "\""    { debug "DTYPE" d;          token lexbuf }
        | "\"" (scope as s) "\""    { debug "SCOPE" s;          token lexbuf }
        (* note: names and files are allowed to have references *)
        | "\"" (name as n) "\""*    { debug "NAME" n;           token lexbuf }
        | "\"" (file as f) "\""*    { debug "FILE" f;           token lexbuf }
        (* note: a reference always appears as a suffix to a name or file *)
        | "|" (name as r) "\""      { debug "REF" r;            token lexbuf }
        | "\"" (cmp_op as o) "\""   { debug "COMPOPR" o;        token lexbuf }
        | "\"" (bw_op as o) "\""    { debug "BITWOPR" o;        token lexbuf }
        | "\"" (boolean as b) "\""  { debug "BOOL" b;           token lexbuf }
        | "\"" (flt_pt as f) "\""   { debug "FLOAT" f;          token lexbuf }
        | "\"" (hex as h) "\""      { debug "HEX" h;            token lexbuf }
        | "\"" (dec as d) "\""      { debug "DEC" d;            token lexbuf }
        | "\"" (oct as o) "\""      { debug "OCT" o;            token lexbuf }
        | "\"" (bin as b) "\""      { debug "BIN" b;            token lexbuf }
    (* Extras: The following are tokens for other values *)
        | ws                        { (* No action *)           token lexbuf }
        | nl                        { Lexing.new_line lexbuf;   token lexbuf }
        (* This is here to allow anything between attribute tags to work *)
        | _                         { xml_warning lexbuf;       token lexbuf }
        | eof                       { (* Exit the program *)    exit 0 }
(* Comment sub-rule: search for matching comment tag.
 * If a different comment tag type found, then continue,
 * else return to token scanner.*)
and comm ctype =
    parse "-->" { if ctype = "<!--" then token lexbuf else comm ctype lexbuf }
        | "?>"  { if ctype = "<?"   then token lexbuf else comm ctype lexbuf }
        | "]>"  { if ctype = "<!["  then token lexbuf else comm ctype lexbuf }
        | nl    { Lexing.new_line lexbuf;                  comm ctype lexbuf }
        | _     { (* Skip everything else *)               comm ctype lexbuf }
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
