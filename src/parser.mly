%{
    open Ast
    open Printf
    (* Define errors *)
    exception XML_Parsing_Error of string
%}

%token <string> OTAG CTAG ATTR
%token <Ast.value> NAME FILE OPR SCOPE REF DTYPE
%token <string> BOOL FLOAT HEX DEC OCT BIN

%left NAME REF FILE OPR SCOPE DTYPE
%left FLOAT HEX DEC OCT BIN BOOL
%left OTAG CTAG ATTR

%start xml_tree
%type <Ast.xml_obj list> xml_tree
%type <Ast.xml_obj list> xml_list
%type <Ast.xml_obj>      xml_obj

%%

xml_tree:
      xml_list                      { $1 }

xml_obj:
      OTAG attr_list CTAG           { { tagname = $1; 
                                        attributes = $2;
                                        inner_objs = [] } }
    | OTAG attr_list xml_list CTAG  { { tagname = $1; 
                                        attributes = $2; 
                                        inner_objs = $3 } }

xml_list:
      xml_obj                       { [ $1 ] }
    | xml_list xml_obj              { $2 :: $1 }

attr_list: 
      attr             { [$1] }
    | attr_list attr   { $2 :: $1 }

attr:
    ATTR value         { {aname = $1; avalue = $2} }

value: 
      NAME             { $1 }
    | FILE             { $1 }
    | ref_list         { $1 }
    | literal          { $1 }
    | OPR              { $1 }
    | SCOPE            { $1 }
    | DTYPE            { $1 }

ref_list: 
      FILE REF         { [$2; $1] }
    | ref_list REF     { $2 :: $1 }

literal: 
      BOOL             { bool_of_string $1}
    | FLOAT            { float_of_string $1}
    | HEX              { int_of_string $1}
    | DEC              { int_of_string $1}
    | OCT              { int_of_string $1}
    | BIN              { int_of_string $1}
