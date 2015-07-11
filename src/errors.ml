open Lexing
open Parsing

(* Define errors *)
let issue msg start finish  = 
        Printf.sprintf "(line %d: char %d..%d): %s" 
                (start.pos_lnum)
                (start.pos_cnum - start.pos_bol) 
                (finish.pos_cnum - finish.pos_bol)
                msg
exception XML_Error of string
let xml_error lexbuf = raise
                (XML_Error
                    (issue 
                        ("Badly Formatted XML: " ^ (Lexing.lexeme lexbuf))
                        (Lexing.lexeme_start_p lexbuf) 
                        (Lexing.lexeme_end_p lexbuf)
                    )
                )
let xml_warning lexbuf = ignore 
                    (issue 
                        ("Warning -- Skipping XML: " ^ (Lexing.lexeme lexbuf))
                        (Lexing.lexeme_start_p lexbuf) 
                        (Lexing.lexeme_end_p lexbuf)
                    )
exception XML_Parse_Error of string
let xml_parse_error nterm = raise
                (XML_Parse_Error
                    (issue 
                        ("Badly Formatted XML")
                        (rhs_start_pos nterm) 
                        (rhs_end_pos nterm)
                    )
                )
