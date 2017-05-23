open Dolmen
open Dolmen.Statement
open Dolmen.Term
open Dolmen.Id 
module P = Smtlib.Make(ParseLocation)(Id)(Term)(Statement)
exception Finish
let parsing file = P.parse_file file

let rec translate_st cont i k = function
    [] -> cont
  | x :: q -> begin
      match x.descr with 
      | Statement.Set_info(str, t) ->
         begin match t with
         | None ->  translate_st cont i (k+1) q
         | Some id ->
            begin match id.term with  
            | Term.Symbol s ->
               if String.equal (s.name) "sat" then  
                 Printf.printf "status : SAT "
               else if String.equal (s.name) "unsat" then 
                 Printf.printf "status : UNSAT "
               else ();
            | _ ->  Printf.printf "no status "  end;
             translate_st cont i (k+1) q  end 
      | Statement.Prove -> translate_st cont i (k+1) q
      | Statement.Exit -> translate_st cont i (k+1) q
      | Statement.Antecedent t  when k > i ->
         let te = (translate_term t) in 
         let te2 = translate_st te i (k+1) q in            
         ClAst.And(cont,ClAst.And(te,te2))
      | _ -> translate_st cont i (k+1) q end 
                                                
and translate_term (t : Term.t) =
  match t.term with
  | Term.Builtin form -> begin
     match form with 
     | 	Term.True ->  (ClAst.True)  
     | 	Term.False -> (ClAst.False)
     | 	Term.Not ->
         ClAst.Not(translate_term (List.nth t.attr 0))
     | 	Term.And ->
         let f = List.fold_left (fun andlst x -> (fun y -> ClAst.And(andlst (translate_term x),y)) )
                                (fun x -> ClAst.And(translate_term (List.nth t.attr 0),x)) (List.tl t.attr) in
        begin match f ClAst.True with
           ClAst.And(x,y) -> x
         | _ -> assert false end
     | 	Term.Or  ->
         let f = List.fold_left (fun andlst x -> (fun y -> ClAst.Or(andlst (translate_term x),y)) )
                                (fun x -> ClAst.Or(translate_term (List.nth t.attr 0),x)) (List.tl t.attr) in
        begin match f ClAst.True with
           ClAst.Or(x,y) -> x
         | _ -> assert false end
     | Term.Imply ->
        ClAst.Or(ClAst.Not(translate_term (List.nth t.attr 0)),translate_term (List.nth t.attr 1))
     | e -> ClAst.Atom(translate_term_term t)
    end
  | Term.App(s,formlst) -> begin 
      match (translate_term_term s) with
      | ClAst.Id("not") ->
         ClAst.Not(translate_term (List.nth formlst 0))
      | ClAst.Id("and") ->
         let f = List.fold_left (fun andlst x -> (fun y -> ClAst.And(andlst (translate_term x),y)) )
                                (fun x -> ClAst.And(translate_term (List.nth formlst 0),x)) (List.tl formlst) in
        begin match f ClAst.True with
           ClAst.And(x,y) -> x
         | _ -> assert false end
      | ClAst.Id("or") ->
         let f = List.fold_left (fun andlst x -> (fun y -> ClAst.Or(andlst (translate_term x),y)) )
                                (fun x -> ClAst.Or(translate_term (List.nth formlst 0),x)) (List.tl formlst) in
        begin match f ClAst.True with
           ClAst.Or(x,y) -> x
         | _ -> assert false end
      | _ -> ClAst.Atom(translate_term_term t) 
    end
                       
  | e -> ClAst.Atom(translate_term_term t)
  
       
       
and translate_term_term t =
  match t.term with
  | Term.Symbol s -> begin
      if (translate_id s.ns) then
        ClAst.Id(s.name)
      else assert false end
  | Term.Builtin form -> begin
      match form with 
      
      | Term.Eq  ->
         ClAst.Eq(translate_term_term (List.nth t.attr 0) ,translate_term_term (List.nth t.attr 1) )
      | Term.Prop -> assert false 
      | _ -> assert false
    end
  | Term.App(s,formlst) when List.length formlst = 0 ->  (translate_term_term s)                   
  | Term.App(s,formlst) -> begin 
     match (translate_term_term s) with
     | ClAst.Id("=") ->
        ClAst.Eq(translate_term_term (List.nth formlst 0) ,translate_term_term (List.nth formlst 1) )
     | _ -> 
        ClAst.App(translate_term_term s,List.map translate_term_term formlst) end
  | _ -> assert false
       
and translate_id  = function
  | Id.Term -> true    
  | _ -> false
       
let rec find_first i = function
    [] -> assert false
  | x :: q -> begin match x.descr with 
    | Statement.Antecedent t  ->
       (translate_term t),i
    | _ -> find_first (i+1) q end
