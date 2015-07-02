%{
    open Ast
    open Printf
%}

%token <string> OTAG NAME CTAG
%token CONN
%token <string> ATTR
%token <string> FILE OPR SCOPE REF DTYPE
%token <string> BOOL FLOAT HEX DEC OCT BIN


%left NAME FILE CONN OPR SCOPE REF DTYPE
%left FLOAT HEX DEC OCT BIN BOOL
%left OTAG CTAG ATTR

%start block
%type <unit> block
%%

block: OTAG attr_list CTAG connection_list { printf "Block contains attrs"}
     | OTAG block_list CTAG { printf "Block contains blocks"}

block_list: 
      block                 { [$1] }
    | block_list block      { $2 :: $1 }

connection_list:
      CONN NAME { 
    | CONN NAME REF

attr: 
      ATTR NAME             { printf "Name %s for attr %s\n" $2 $1;
                                ($1, $2)}
    | ATTR FILE             { printf "Filename %s for attr %s\n" $2 $1;
                                ($1, $2)}
    | ATTR FILE ref_list    { printf "Block %s in file %s for attr %s\n" 
                                $3 $2 $1; 
                                ($1, $2)}
    | ATTR value            { printf "Value %s for attr %s\n" $2 $1;
                                ($1, $2)}
    | ATTR OPR              { printf "Operator %s for attr %s\n" $2 $1; 
                                ($1, $2)}
    | ATTR SCOPE            { printf "Scope %s for attr %s\n" $2 $1; 
                                ($1, $2)}
    | ATTR DTYPE            { printf "Datatype %s for attr %s\n" $2 $1; 
                                ($1, $2)}

attr_list: 
      attr                  { [$1] }
    | attr_list attr        { $1 :: $2 }

ref_list: 
      REF                   { [$1] }
    | ref_list REF          { $2 :: $1 }

value: 
      BOOL                  { printf "Bool value %s" $1; $1}
    | FLOAT                 { printf "Float value %s" $1; $1}
    | HEX                   { printf "Hex value %s" $1; $1}
    | DEC                   { printf "Dec value %s" $1; $1}
    | OCT                   { printf "Oct value %s" $1; $1}
    | BIN                   { printf "Bin value %s" $1; $1}
