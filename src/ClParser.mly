%token AND
%token TRUE
%token FALSE
%token NOT
%token OR EQ COMMA
%token LPAREN RPAREN
%token <string> BID
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
  (* | AT term term { ClAst.BApp ($2,$3) } *)
  | term  LPAREN separated_list (COMMA, term) RPAREN { ClAst.App ($1,$3) }
  | LPAREN term RPAREN  { $2 }

  
id:
  | BID       { ClAst.Id($1) }
           
eqterm:
  | term EQ term  { ClAst.Eq ($1,$3) }
