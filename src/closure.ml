open ClAst


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

       
let get_formula =
  
  let fichier = match !fichiera with None -> assert false | Some(fichier) -> fichier in 
  if Sys.file_exists fichier then
   begin
     let contenu = ref "" in
     let _ = lit fichier contenu in
     let ast =  (parse (!contenu)) in
     ClPrinter.interpPrint ast;
     ast
   end
  else(
    print_string "le fichier n existe pas\n";
    exit(1)
  )

