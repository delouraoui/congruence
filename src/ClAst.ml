
type clForm =
  | False
  | True
  | Atom of term
  | Not of  clForm 
  | And of clForm * clForm 
  | Or of clForm * clForm

           
and term =
   | Id of string
   | Eq of term  * term
   | App of term  * (term list)

