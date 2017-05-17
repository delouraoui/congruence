open ClAst


let rec to_string exp =
  let print_paren exp =
    match exp with
      | True | False | Atom(_) | Not(_)
          -> to_string exp
      | _ -> "(" ^ (to_string exp) ^ ")"
  in
  match exp with
    | True           -> "⊤"
    | False          -> "⊥"
    | Atom(p)        -> print_term p
    | Not(exp)       -> "¬" ^ (print_paren exp)
    | And(l, r)      -> (print_paren l) ^ " ∧ " ^ (print_paren r)
    | Or(l, r)       -> (print_paren l) ^ " ∨ " ^ (print_paren r)

and print_term = function
  | App (a,b) -> "(" ^ (print_term a) ^" "^ (List.fold_right (fun y str -> str^(print_term y) ) b "")^ ")"
  | Eq  (a,b) -> (print_term a) ^" = "^ (print_term b)
  | Id x -> x
          
let interpPrint a = 
	print_string (to_string a)(* ;print_string "\n" *)
