open Lexing
open Parsing
open Ast

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
let xml_parse_error nterm msg = raise
                (XML_Parse_Error
                    (issue 
                        ("Badly Formatted XML: " ^ msg)
                        (rhs_start_pos nterm) 
                        (rhs_end_pos nterm)
                    )
                )

exception Block_Error of string
let block_error blk msg = raise
                (Block_Error
                    (msg ^ " for block:\n" ^ Ast.string_of_xml blk)
                )
