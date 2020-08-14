%{
    open Unix
    open Printf
    open Pervasives
    open IniBase
%}

%token <string> QUOTE
%token BCTX ECTX EQ EOL
%token <string> TOKEN
%token EOF

%type <string * string> pair
%type <string> context
%type <IniBase.line> main

%start main pair context

%%

pair :
  | TOKEN EQ QUOTE EOL                     { ($1, $3) }
  | TOKEN EQ TOKEN EOL                     { ($1, $3) }
  | TOKEN EQ EOL                           { ($1, "") }
;

context :
  | BCTX TOKEN ECTX EOL                    { $2 }
;

main :
  | context                                 { IniBase.Context $1 }
  | pair                                    { IniBase.Pair $1 }
  | EOL main                                { $2 }
  | EOF                                     { Eof }
;

%%

