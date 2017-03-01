%token <int> INT
%token <float> FLOAT
%token <string> SYMBOL
%token <string> STRING
%token LEFT_PAR
%token RIGHT_PAR
%token EOF

%start <Lisp.t option> main
%%

main:
  | EOF		{ None }
  | v = value	{ Some v }
  ;

value:
  | LEFT_PAR; vals = values; RIGHT_PAR
    { `List vals }
  | LEFT_PAR; RIGHT_PAR
    { `List [] }
  | s = SYMBOL
    { `Symbol s }
  | s = STRING
    { `String s }
  | num = INT
    { `Int num }
  | num = FLOAT
    { `Float num }
  ;

values:
  v = rev_values { List.rev v }
  ;

rev_values:
  | { [] }
  | rest = rev_values; v = value
    { v :: rest }
  ;

