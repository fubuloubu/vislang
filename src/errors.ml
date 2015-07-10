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
