open ClAst
       
type eq =
  Equal of term * term
        
type pending = eq list
type rep_table = (term * term) list
type class_lst = (term * term list) list
type lookup_tbl = (term * term) list
type use_lst = (term * eq list) list

let get_ulist_of (a:term) (ulist : use_lst) =
  try Some (List.assoc a ulist)  with Not_found -> None
                                                   
let get_clst a clst = 
  try List.assoc a clst  with Not_found -> assert false
                           
let get_rep_of a repr = 
  try List.assoc a repr  with Not_found -> assert false 
                                                  
let lookup (a:term) (tbl : lookup_tbl) : term =
  try List.assoc a tbl with Not_found ->  assert false

let rec eq_term a b =
  match a, b with
  | Id a', Id b' -> String.equal a' b'
  | App(a,b), App(a',b') ->
     eq_term a b && eq_term a' b'
  | Eq(a,b),Eq(a',b') -> 
     eq_term a b && eq_term a' b'
  | _,_ -> false
                  
let rec set_reptbl c b = function
  | [] -> []
  | (c',b')::l when (eq_term c c') ->
     (c,b)::(set_reptbl c b l)
  | (c',b')::l ->
     (c',b')::(set_reptbl c b l)
                
let rec set_looktbl c d e' : lookup_tbl -> lookup_tbl = function
  | [] -> []
  | (App(c',d'),b')::l when (eq_term c c') && (eq_term d d') ->
     (App(c',d'),e')::(set_looktbl c d e' l)
  | (c',b')::l ->
     (c',b')::(set_looktbl c d e' l)
                
let rec set_ulst a eq = function
  | [] -> []
  | (a',b')::l when (eq_term a a') ->
     (a',eq::b')::(set_ulst a eq l)
  | (a',b')::l ->
     (a',b')::(set_ulst a eq l)

                
let rec add_clst_of b c = function
  | [] -> []
  | (rep,lst)::l when (eq_term b rep) ->
     (rep,c::lst)::add_clst_of b c l
  | (rep,lst)::l ->
     (rep,lst)::add_clst_of b c l              
  

let rec on_ulist  rep_table pending lookuptbl ulstb = function
  | [] -> pending, lookuptbl, ulstb
  | Equal(App(c,d),e)::l ->
       let e' = (get_rep_of e rep_table) in
       let c' = get_rep_of c rep_table in
       let d' = get_rep_of d rep_table in
       let pending = 
       begin 
         match lookup (App(c',d')) lookuptbl with
         | App(f,_) when not (eq_term (get_rep_of f rep_table) e')  ->
            (Equal((get_rep_of f rep_table),e'))::pending
         | _ -> pending 
       end in
       let lookuptbl = set_looktbl c' d' e' lookuptbl in
       let ulstb = Equal(App(c,d),e) :: ulstb in
       on_ulist  rep_table pending lookuptbl ulstb l
    | a::l -> on_ulist  rep_table pending lookuptbl ulstb l

let unfold = function
  | Some l -> l
  | None -> raise Not_found
  
                        
let rec cc rep_table clst userlst lookuptbl = function
  | [] -> userlst
  | (Equal(a,b)) :: l ->
     let a' = get_rep_of a rep_table in
     let b' = get_rep_of b rep_table in
     let cmp = List.length (get_clst a' clst) < List.length (get_clst b' clst) in
     if not (eq_term a' b') && cmp then begin
       let rep_table =
         List.fold_left (fun reptbl c ->
             set_reptbl c b' reptbl ) rep_table (get_clst a' clst)  in
       let clst = List.fold_left (fun clst' c ->
                      (add_clst_of b c clst') ) clst (get_clst a' clst) in
       let pending,lookuptbl,ulstb =
         let userlstb' = unfold (get_ulist_of b' userlst) in
         let userlsta' = unfold (get_ulist_of a' userlst) in
         on_ulist rep_table l lookuptbl userlstb'  userlsta' in
       cc rep_table clst userlst lookuptbl (l@pending)
       end
     else cc rep_table clst userlst lookuptbl l

let addulst a eq ulst = 
  try
    let _ = unfold (get_ulist_of a ulst) in
    set_ulst a eq ulst
  with Not_found -> (a,[eq])::ulst
                                
let addlklst a b c lookuptbl = 
  try
    let _ = List.assoc (App(a,b)) lookuptbl in
    set_looktbl a b c lookuptbl
  with Not_found -> (App(a,b),c)::lookuptbl      
               
let rec init pending ulst lookuptbl = function
  | [] -> pending,ulst,lookuptbl
  | Eq(a,b)::l ->
     begin
       match a with
       | App(e1,e2) ->
          let ulst = addulst e1 (Equal(a,b)) ulst in
          let ulst = addulst e2 (Equal(a,b)) ulst in
          let lookuptbl = addlklst e1 e2 b lookuptbl in
          (init (Equal(a,b)::pending) ulst lookuptbl l)
       | _ -> 
           (init (Equal(a,b)::pending) ulst lookuptbl l)
     end
  | _ :: l -> (init  pending ulst lookuptbl l)

let congurence cnstlst eq_lst =
  let pending,ulst,lookuptbl = init [] [] [] eq_lst in
  let clst = List.map (fun (a,la) -> (a,[la]) ) cnstlst in
  cc cnstlst clst ulst lookuptbl pending 
                


       
