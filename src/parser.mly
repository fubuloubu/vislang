%{
    open Ast
    open Printf
%}

%token C_ELEM EOF
%token <string> O_ELEM ATTR
%token <string> NAME FILE REF DTYPE SCOPE
%token GRT LST EQT NEQ LEQ GEQ
%token OR AND NOT XOR NAND NOR XNOR
%token <string> BOOL FLOAT HEX DEC OCT BIN

%left SCOPE DTYPE NAME FILE REF
%left BOOL FLOAT HEX DEC OCT BIN
%left O_ELEM C_ELEM ELEM ATTR

%start xml_tree
%type <Ast.xml_obj>      xml_tree
%type <Ast.xml_obj list> xml_list
%type <Ast.xml_obj>      xml_obj

%%

xml_tree:
    xml_obj EOF { $1 }

xml_obj:
      O_ELEM attr_list C_ELEM           { { blkname     = $1 ; 
                                            attributes  = $2 ;
                                            inner_objs  = [] } }
    | O_ELEM attr_list xml_list C_ELEM  { { blkname     = $1 ; 
                                            attributes  = $2 ; 
                                            inner_objs  = $3 } }

xml_list:
      xml_obj           { [  $1  ] }
    | xml_list xml_obj  { $2 :: $1 }

attr_list: 
      attr              { [  $1  ] }
    | attr_list attr    { $2 :: $1 }

attr:
      ATTR value        { { aname   = $1 ; 
                            avalue  = $2 } }

value: 
      ref               { Ref       ($1) }
    | literal           {            $1  }
    | bitwopr           { Bitwopr   ($1) }
    | compopr           { Compopr   ($1) }
    | SCOPE             { Datatype  ($1) }
    | DTYPE             { Scope     ($1) }

ref:
      FILE ref_list     { { reftype = "FILE" ; 
                            refroot = $1     ;
                            reflist = $2     } }
    | NAME ref_list     { { reftype = "NAME" ;
                            refroot = $1     ;
                            reflist = $2     } }

ref_list: 
      REF               { [  $1  ] }
    | ref_list REF      { $2 :: $1 }

literal: 
      BOOL              { Bool  (bool_of_string  $1) }
    | FLOAT             { Float (float_of_string $1) }
    | HEX               { Int   (int_of_string   $1) }
    | DEC               { Int   (int_of_string   $1) }
    | OCT               { Int   (int_of_string   $1) }
    | BIN               { Int   (int_of_string   $1) }

bitwopr:
      OR                { Or    }
    | AND               { And   } 
    | NOT               { Not   }
    | XOR               { Xor   }
    | NAND              { Nand  }
    | NOR               { Nor   }
    | XNOR              { Xnor  }

compopr:
      GRT               { Grt   }
    | LST               { Lst   } 
    | EQT               { Eqt   }
    | NEQ               { Neq   }
    | LEQ               { Leq   }
    | GEQ               { Geq   }
