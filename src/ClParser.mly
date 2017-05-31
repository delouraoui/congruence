%token AND
%token TRUE
%token FALSE
%token NOT COLON
%token OR EQ COMMA 
%token LPAREN RPAREN LBRAQ RBRAQ
%token <string> BID
%token <int> NUM
%token EOF

%left AND OR

%start main             /* the entry point */
%type <ClAst.clForm> main
%%
main:
    clForm EOF              { $1 }
;


clForm:
  | FALSE                               { ClAst.False }
  | TRUE                                { ClAst.True }
  | NOT LPAREN clForm RPAREN            { ClAst.Not ($3) }
  | clForm AND clForm                   { ClAst.And ($1, $3) }
  | clForm OR clForm                    { ClAst.Or ($1,$3) }
  | eqterm                              { ClAst.Atom($1) }
  | term                                { ClAst.Atom($1) }
  
term:
  | id { $1 }
  | term  LPAREN separated_list (COMMA, term) RPAREN { ClAst.App ($1,$3) }
  | term  LBRAQ NUM COLON NUM RBRAQ LPAREN separated_list (COMMA, term) RPAREN
          { ClAst.PApp ($3,$5,$1,$8) }
  | LPAREN term RPAREN  { $2 }

  
id:
  | BID       { ClAst.Id($1) }
          
eqterm:
  | term EQ term  { ClAst.Eq ($1,$3) }
