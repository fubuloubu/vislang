{
    open Xparser
    open Errors
}
(* Main definitions for use below *)
let ws      = [' ' '\t']
let nl      = ['\r' '\n']
let tag     = ( "BLOCK" 
            |   "REFERENCE" 
            |   "CONNECTION"
            |   "INPUT"
            |   "OUTPUT" 
            |   "CONSTANT" 
            |   "SIGNAL" 
            |   "CAST"
            |   "MEM"
            |   "DT"
            |   "NOT"
            |   "AND"
            |   "OR"
            |   "NOR"
            |   "NAND"
            |   "XOR"
            |   "BITWISE"
            |   "IF"
            |   "COMPARE"
            |   "SUM"
            |   "PROD"
            |   "GAIN"
            |   "INV"
            |   "MUX"
            |   "DEMUX"
            |   "STRUCT"
            |   "DESTRUCT"
            |   "MAP"
            |   "FILTER"
            |   "REDUCE"
            ) (* all accepted tags *)
let attr    = ( "name"
            |   "ref"
            |   "datatype"
            |   "to"
            |   "from"
            |   "ic"
            |   "operation"
            |   "value"
            ) (* all accepted attributes *)
let name    = ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let datatype= ( "auto" 
            |   "double" | "single" 
            |   "boolean" 
            |   'u'? "int" ("8" | "16" | "32") (* all integer types *)
            (*|   name (* for structs *)*)
            )
let file    = ( ".." | ".")? ("/" ['A'-'Z' 'a'-'z' '0'-'9' '_' '-' '.']+ )+ ".vl"
let sign    = ( "+" | "-")
let boolean = ( "true" | "false")
let digit   = [ '0' - '9' ]
let flt_pt  = sign? ( digit+ "." digit* | "." digit+ )
let hex     = sign? '0' ['x' 'X'] ['A'-'F' 'a'-'f' '0'-'9']+
let oct     = sign? '0' ['o' 'O'] ['0'-'7']+
let bin     = sign? '0' ['b' 'B'] ['0' '1']+
let dec     = sign? digit+ (* Allow signed integers for any encoding *)

(* Main scanner step: search for elements, attributes, and comments *)
rule token =
    parse
    (* Comments: Search for any of the following ignored tag openings, 
     * then jump to rule for parsing an ignore anything inside it. *) 
        "<?"   | (* XML Declarator  *)
        "<!--" | (* XML Comments    *)
        "<!["    (* DOCTYPE Markup  *)
        as ctype                        { comm ctype lexbuf }
    (* Elements: Scan for supported blocks and link to parsing stage. 
     * If an unsupported block is found, note it as information for compilation *)
        | "<"  "vl:" (tag as t)         { O_ELEM( t )  }
        | "</" "vl:" (tag as t) ">"     { C_ELEM( t )  }
        | "/>"                          { E_ELEM       }
        | ">"   (* No tag required *)   { token lexbuf }
    (* Attributes: The following are tokens for different values
     * attributes might take on. *)
        | attr as a "="             { ATTR  ( a ) }
        | "\"" (datatype as d) "\"" { DTYPE ( d ) }
        (* note: names and files are allowed to have references *)  
        | "\"" (name as n) "\""?    { NAME  ( n ) } (* TODO: this syntax allows a  *)
        | "\"" (file as f) "\""?    { FILE  ( f ) } (*   quote character inside it *)
        (* note: a reference always appears as a suffix to a name or file *)
        | "|"  (name as r) "\""?    { REF   ( r ) } (* need some method to denote end*)
        (* Comparision Operators *)
        | "\"" "==" "\""            { EQT }
        | "\"" ">"  "\""            { GRT }
        | "\"" "<"  "\""            { LST }
        | "\"" ">=" "\""            { GEQ }
        | "\"" "<=" "\""            { LEQ }
        | "\"" "!=" "\""            { NEQ }
        (* Bitwise Operators *)
        | "\"" "or"   "\""          { OR   }
        | "\"" "and"  "\""          { AND  }
        | "\"" "not"  "\""          { NOT  }
        | "\"" "xor"  "\""          { XOR  }
        | "\"" "nor"  "\""          { NOR  }
        | "\"" "nand" "\""          { NAND }
        | "\"" "xnor" "\""          { XNOR }
        (* Literals *)
        | "\"" (boolean as b) "\""  { BOOL  ( b ) }
        | "\"" (flt_pt as f) "\""   { FLOAT ( f ) }
        | "\"" (hex as h) "\""      { HEX   ( h ) }
        | "\"" (dec as d) "\""      { DEC   ( d ) }
        | "\"" (oct as o) "\""      { OCT   ( o ) }
        | "\"" (bin as b) "\""      { BIN   ( b ) }
    (* Extras: The following are tokens for other values *)
        | ws                        { token lexbuf }
        | nl                        { Lexing.new_line lexbuf; 
                                      token lexbuf }
        (* This is here to allow anything between attribute tags to work *)
        | _                         { xml_warning lexbuf;
                                      token lexbuf }
        | eof                       { EOF }
(* Comment sub-rule: search for matching comment tag.
 * If a different comment tag type found, then continue,
 * else return to token scanner.*)
and comm ctype =
    parse "-->" { if ctype = "<!--" then token lexbuf else comm ctype lexbuf }
        | "?>"  { if ctype = "<?"   then token lexbuf else comm ctype lexbuf }
        | "]>"  { if ctype = "<!["  then token lexbuf else comm ctype lexbuf }
        | nl    { Lexing.new_line lexbuf;                  comm ctype lexbuf }
        | _     { (* Skip everything else *)               comm ctype lexbuf }
