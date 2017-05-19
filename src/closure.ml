open ClAst


(*** Termes traitement ***)
       
let rec get_cnst_form = function
  | True           -> [(Id ("True"))]
  | False          -> [(Id ("False"))]
  | Atom(p)        -> get_cnst_term p
  | Not(exp)       -> get_cnst_form exp
  | And(l, r)      -> (get_cnst_form l) @ (get_cnst_form r)
  | Or(l, r)       -> (get_cnst_form l) @ (get_cnst_form r)
                                                  
and get_cnst_term = function
  | App (a,b) -> (App (a,b)) ::  (* (get_cnst_term a) @ *) (List.flatten (List.map get_cnst_term b))
  | Eq  (a,b) -> (get_cnst_term a) @ (get_cnst_term b)
  | Id x ->  [(Id x)]
              
let rec get_eq_form = function
  | True           -> []
  | False          -> []
  | Atom(p)        -> get_eq_term p
  | Not(exp)       -> [](* get_eq_form exp *)
  | And(l, r)      -> (get_eq_form l) @ (get_eq_form r)
  | Or(l, r)       -> (get_eq_form l) @ (get_eq_form r)
                                                  
and get_eq_term = function
  | Eq  (a,b)  -> [(a,b)]
  | _ -> []

          
let rec get_neq_form = function
  | True               -> []
  | False              -> []
  | Not(Atom(Eq(a,b))) -> [(a,b)]  
  | And(l, r)          -> (get_neq_form l) @ (get_neq_form r)
  | Or(l, r)           -> (get_neq_form l) @ (get_neq_form r)
  | _ -> []                                                

          
(*** Administrative part ***)
       
(* Fonction permetant de récupérer l'ast généré après parsing *)
let parse (s : string) : clForm = ClParser.main ClLexer.token (Lexing.from_string s)

let lit name fichier = 
  let buf = open_in name in 
  try
    while true do
      fichier := (!fichier)^ (input_line (buf) )
    done
  with End_of_file -> ()

let fichiera = ref None
                    
let arg_spec =
  ["-f", Arg.String (fun i -> fichiera := Some i ),""]
  

let () = Arg.parse arg_spec (fun _ -> ()) "Usage ./closure -f <examplefile>"
(*** Administrative part ***)

                   
let get_formula =  
  let fichier = match !fichiera with None -> assert false | Some(fichier) -> fichier in 
  if Sys.file_exists fichier then
   begin
     let contenu = ref "" in
     let _ = lit fichier contenu in
     let ast =  (parse (!contenu)) in
     (* let closure1 = *)
     (*   UnionFind.congrClosure *)
     (*     (get_eq_form ast) *)
     (*     (get_cnst_form ast) *)
     (*     (get_neq_form ast) in *)
     let closure2 =
       UfOtherImpl.decision
       (get_cnst_form ast)
       (get_eq_form ast)
       (get_neq_form ast) in
     
     (* Printf.printf "First Implementation \n"; *)
     (* if closure1 then Printf.printf "SAT\n" *)
     (* else Printf.printf "UNSAT\n"; *)
     Printf.printf "Seconde Implementation \n";
     if closure2 then Printf.printf "SAT\n"
     else Printf.printf "UNSAT\n";
     ClPrinter.interpPrint ast; Printf.printf " \n";
     ast
   end
  else(
    print_string "le fichier n existe pas\n";
    exit(1)
  )

  
