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
    | Not(exp)       -> "¬ (" ^ (print_paren exp) ^ ")"
    | And(l, r)      -> (print_paren l) ^ " ∧ " ^ (print_paren r)
    | Or(l, r)       -> (print_paren l) ^ " ∨ " ^ (print_paren r)

and print_term = function
  | PApp (applied,tot,a,b) ->
     (print_term a) ^"^["^(string_of_int applied)^"/"^(string_of_int tot)^"] ("^
       (List.fold_left (fun str y ->
            if String.equal str "" then 
              str^(print_term y)
            else str^","^(print_term y)
          ) "" b )^ ")"
  | App (a,b) -> (print_term a) ^"("^ (List.fold_left (fun str y ->
                                                 if String.equal str "" then 
                                                   str^(print_term y)
                                                 else str^","^(print_term y)
                                               ) "" b )^ ")"
  | Eq  (a,b) -> (print_term a) ^" = "^ (print_term b)
  | Id x -> x
          
let interpPrint a = 
	print_string (to_string a)(* ;print_string "\n" *)
