open ClAst
exception Empty      
type eq =
  Equal of term * term
        
type pending = eq list
type rep_table = (term * term) list
type class_lst = (term * term list) list
type lookup_tbl = (term * term) list
type use_lst = (term * eq list) list

let rec eq_term e1 e2 =
  match e1, e2 with
  | Const a',  Const b' -> String.equal a' b'
  | App( (Id a) ,b), App(Id(a'),b') ->
     String.equal a' a' && (List.for_all2 eq_term b b')
  | Eq(a,b),Eq(a',b') -> 
     eq_term a a' && eq_term b b'
  | _,_ -> false

let rec assoc x = function
  | [] ->  raise Not_found
  | (x',l)::q ->
     if eq_term x x' then l
     else assoc x q
         
             
let get_ulist_of (a:term) (ulist : use_lst) =
  try Some (assoc a ulist)  with Not_found -> None
                                                   
let get_clst a clst = 
  try assoc a clst  with Not_found -> assert false
                           
let get_rep_of a repr = 
  try assoc a repr  with Not_found ->
    assert false 
                                                  
let lookup (a:term) (tbl : lookup_tbl)  =
  try Some (assoc a tbl) with Not_found ->  None
                                             
             

let rec set_reptbl c b = function
  | [] -> []
  | (c',b')::l when (eq_term c c') ->
     (c,b)::(set_reptbl c b l)
  | (c',b')::l ->
     (c',b')::(set_reptbl c b l)
                
let rec set_looktbl c d e' : lookup_tbl -> lookup_tbl = function
  | [] -> []
  | (App((Id c'),d'),b')::l when (String.equal c c') && (List.for_all2 eq_term d d') ->
     (App((Id c'),d'),e')::(set_looktbl c d e' l)
  | (c',b')::l ->
     (c',b')::(set_looktbl c d e' l)
                
let rec set_ulst a eq = function
  | [] -> []
  | (a',b')::l when (eq_term a a') ->
     (a',eq::b')::(set_ulst a eq l)
  | (a',b')::l ->
     (a',b')::(set_ulst a eq l)
    
let rec updt_ulst a eq = function
  | [] -> []
  | (a',b')::l when (eq_term a a') ->
     (a',eq@b')::(updt_ulst a eq l)
  | (a',b')::l ->
     (a',b')::(updt_ulst a eq l)
                
let rec add_clst_of b c = function
  | [] ->  []
  | (rep,lst)::l when (eq_term b rep) ->
     (rep,c::lst)::add_clst_of b c l
  | (rep,lst)::l ->
     (rep,lst)::add_clst_of b c l              
  

let rec on_ulist  rep_table pending lookuptbl ulstb = function
  | [] ->   pending, lookuptbl, ulstb
  | Equal(App(c,d),e)::l ->
       let e' = (get_rep_of e rep_table) in
       let c' = get_rep_of c rep_table in
       let d' = get_rep_of d rep_table in
       let pending = begin 
         match lookup (App(c',d')) lookuptbl with
         | Some(App(f,_)) when not (eq_term (get_rep_of f rep_table) e')  ->
            (Equal(e',(get_rep_of f rep_table)))::pending
         | _ ->  pending 
         end in
       
       let lookuptbl = set_looktbl c' d' e' lookuptbl in
       let ulstb = Equal(App(c,d),e) :: ulstb in
       on_ulist rep_table pending lookuptbl ulstb l
    | a::l -> on_ulist  rep_table pending lookuptbl ulstb l

let unfold = function
  | Some l -> l
  | None -> raise Empty
  
                        
let rec cc rep_table clst userlst lookuptbl = function
  | [] -> clst, userlst 
  | (Equal(a,b)) :: l ->     
     let a' = get_rep_of a rep_table in
     let b' = get_rep_of b rep_table in
     let cmp = List.length (get_clst a' clst) <= List.length (get_clst b' clst) in
     if not (eq_term a' b') && cmp then begin
       let rep_table =
         List.fold_left (fun reptbl c -> 
             set_reptbl c b' reptbl ) rep_table (get_clst a' clst)  in
       let clst = List.fold_left (fun clst' c -> 
                      (add_clst_of b c clst') ) clst (get_clst a' clst) in
       try 
         let pending,lookuptbl,ulstb =
           let userlstb' =
             try unfold (get_ulist_of b' userlst) with Empty ->   [] in
           let userlsta' =  unfold (get_ulist_of a' userlst) in
           on_ulist rep_table l lookuptbl userlstb'  userlsta' in
         let userlst =
           try 
               let _ = unfold (get_ulist_of b' userlst)  in
             (updt_ulst b' ulstb  userlst)
           with Empty ->  (b',ulstb)::userlst  in
         cc rep_table clst  userlst lookuptbl (l@pending)
       with Empty ->  
         cc rep_table clst userlst lookuptbl l
       end
     else cc rep_table clst userlst lookuptbl l

let addulst a eq ulst = 
  try
    let _ = unfold (get_ulist_of a ulst) in
    set_ulst a eq ulst
  with Empty -> (a,[eq])::ulst
                                
let addlklst a b c lookuptbl = 
  try
    let _ = assoc (CApp(a,b)) lookuptbl in
    set_looktbl a b c lookuptbl
  with Not_found -> (CApp(a,b),c)::lookuptbl      
               
let rec init pending1 pending2 ulst lookuptbl = function
  | [] -> (List.rev pending2) @(List.rev pending1),ulst,lookuptbl
  | Eq(a,b)::l ->
     begin
       match a with
       | App(e1,e2) ->
          let ulst = addulst e1 (Equal(a,b)) ulst in
          let ulst = addulst e2 (Equal(a,b)) ulst in
          let lookuptbl = addlklst e1 e2 b lookuptbl in
          (init (Equal(a,b)::pending1) pending2 ulst lookuptbl l)
       | _ ->
           (init pending1 (Equal(a,b)::pending2) ulst lookuptbl l)
     end
  | _ :: l -> (init  pending1 pending2 ulst lookuptbl l)

let rec elim_bdl = function
  | [] -> []
  | (a,b)::q ->
     try
       let _ = assoc a q in
       elim_bdl q
     with e -> (a,b)::elim_bdl q


let rec mem x = function
  | [] -> false
  | x'::q ->
     if eq_term x x' then true
     else mem x q
             
let rec elim_bdl_s = function
  | [] -> []
  | a::q ->
     if mem a q then 
       elim_bdl_s q
     else a::elim_bdl_s q
             
let rec elim_bdl'  = function
  | [] -> []
  | (a,b)::q ->
     let l = elim_bdl_s b in
     (a,l)::elim_bdl' q 
                        
let rec elim_bdl_eq = function
  | [] -> []
  | a::q ->
     if mem a q then elim_bdl_eq q
     else a::elim_bdl_eq q
        
let congurence cnstlst eq_lst =
  let cnstlst = elim_bdl cnstlst in
  let eq_lst  = elim_bdl_eq eq_lst in
  let pending,ulst,lookuptbl = init [] [] [] [] eq_lst in
  let clst = List.map (fun (a,la) -> (a,[la]) ) cnstlst in
  let congr_class, ulist = cc cnstlst clst ulst lookuptbl pending in
  elim_bdl' congr_class
                


       
