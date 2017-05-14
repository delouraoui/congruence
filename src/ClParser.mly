%token AND
%token TRUE
%token FALSE
%token NOT
%token OR EQ
%token LPAREN RPAREN
%token <string> BID
%token EOF

%left AND OR
%nonassoc NOT 

%start main             /* the entry point */
%type <ClAst.clForm> main
%%
main:
    clForm EOF              { $1 }
;

clForm:
  | FALSE                               { ClAst.False }
  | TRUE                                { ClAst.True }
  | NOT clForm                          { ClAst.Not ($2) }
  | clForm AND clForm                   { ClAst.And ($1, $3) }
  | clForm OR clForm                    { ClAst.Or ($1,$3) }
  (* | LPAREN clForm RPAREN                { $2 } *)
  | eqterm                              { ClAst.Atom($1) }
  | term                                { ClAst.Atom($1) }

term:                               
  | id { $1 }
  | term term  { ClAst.App ($1,$2) } 
  | LPAREN term RPAREN  { $2 }
        
id:
  | BID       { ClAst.Id ($1) }
              
eqterm:
  | term EQ term  { ClAst.Eq ($1,$3) }    
