{
   (* open Parser *)
    open Printf

    (* current token line number *)
    let line_num = ref 1

    (* Define errors *)
    exception Bad_xml_error of string
    let bad_xml_error line = raise (Bad_xml_error ("\nBad XML on line " ^ 
                        (string_of_int !line_num) ^ "\n" ^ line ^ "\n"))
}
(* Main definitions for use below *)
let digit = ['0'-'9']
let name  = ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let file  = ("../" | "./" | "/")
            (['A'-'Z' 'a'-'z' '0'-'9' '_' '.']+ ("/")?)+
            (".vl")
let cnx = ("|" name)+
let sign = ("+" | "-")
let flt_pt = sign? (digit+ "." digit* | "." digit+)
let intpfx = ("0x" | "2x" | "8x" | sign)?
let attribute = (' ' name '=' "\""
                (name cnx? | flt_pt | intpfx digit+)+ "\"")

(* Main scanner step: search for blocks and comments *)
rule token =
    parse [' ' '\t']                { token lexbuf }
        | ('\r'|'\n'|"\r\n")        { incr line_num; token lexbuf }
        | ("<?"
        | "<!--") as comment_type   { comment comment_type lexbuf }
        | '<' (name as tag)         { block tag lexbuf }
        | _* as line                { bad_xml_error line }
        | eof                       { printf "Reached end of file\n" }
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
        | ('\r'|'\n'|"\r\n")
                { incr line_num;
                  comment ctype lexbuf }
        | _*     { comment ctype lexbuf }
(* Block sub-rule: Scan for supported blocks and link
 * to parsing stage. If an unsupported block is found, note
 * it as information for compilation *)
and block tag =
    parse attribute* as attributes ("/>" | ">") as blk_end
        (* Note: attributes are only accepted in-tag e.g.:
         * <tag attr1=blah attr2=blah ..>ignore here</tag> *)
        {
            printf "Block %s contains: %s\n" tag attributes;
            if blk_end = ">"
            then closing_tag tag lexbuf
            else token lexbuf
        }
        | _* as issue { bad_xml_error issue }
(* If open tag found without closing tag, run this rule to
 * find the closing tag *)
and closing_tag tag =
    parse "</" name as tag_to_chk ">"
        {   
            printf "Checking if tag %s is closed by tag %s\n"
                tag tag_to_chk;
            if tag_to_chk = tag
            then token lexbuf 
            else closing_tag tag lexbuf }
        | ('\r'|'\n'|"\r\n")
                       { incr line_num; closing_tag tag lexbuf }
        | _* as ignored { printf "Ignoring %s\n" ignored;
                         closing_tag tag lexbuf }
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
