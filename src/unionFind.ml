open ClAst

let rec eq_term e1 e2 =
  match e1, e2 with
  | Id a',  Id b' -> String.equal a' b'
  | App(Id a ,b), App(Id a',b') ->
     String.equal a a' && (List.for_all2 eq_term b b')
  | Eq(a,b),Eq(a',b') -> 
     eq_term a a' && eq_term b b'
  | _,_ -> false

         
let rec assoc x = function
  | [] ->  raise Not_found
  | (x',l)::q ->
     if eq_term x x' then l
     else assoc x q

    
type node = {
    label : term;
    mutable childs : node list;
    mutable parents  : node list;
    mutable parent : node;
  }
          

let rec mem x = function
  | [] -> false
  | x'::q ->
     if eq_term x (x'.label) then true
     else mem x q
    
let rec mem_paires (x,y) = function
  | [] -> false
  | (x',y')::q ->
     if eq_term x.label (x'.label) && eq_term y.label (y'.label) then true
     else mem_paires (x,y) q
    
let rec mem' x = function
  | [] -> false
  | x'::q ->
     if eq_term x.label (x'.label) then true
     else mem' x q
    
let rec elim_bdl n = function
  | [] -> []
  | a::q ->
     if mem' a q then 
       elim_bdl n q
     else if eq_term n.label a.label then
       elim_bdl n q
     else a::elim_bdl n q
    
let rec elim_bdl_paires = function
  | [] -> []
  | (a,b)::q ->
     if mem_paires (a,b) q then 
       elim_bdl_paires q
     else (a,b)::elim_bdl_paires q

    
let inter l1 l2 =
  (List.filter (fun e2 -> mem' e2 l2) l1)@(List.filter (fun e2 -> mem' e2 l1) l2)
                
let empty = []

let rec find m u =
  let n1 = u.parent in
  let n2 = u in 
  if eq_term n1.label n2.label then u,m  (* root *)
  else begin
      let root,m = find m n1 in
      (* path compression *)
      let m =(* Printf.printf " Find u ";ClPrinter.interpPrint (Atom n2.label); Printf.printf " his parent "; *)
            (* ClPrinter.interpPrint (Atom n1.label); Printf.printf " his root "; *)
            (* ClPrinter.interpPrint (Atom root.label); Printf.printf " \n "; *)
            u.parent <- root;
            (n1,root)::m in
      root,(n1,n2)::(n2,root)::m
    end

let union m u v =
  let ru,m = find m u in
  let rv,m = find m v in (* Printf.printf " union "; *)
  if not (eq_term (ru.label) (rv.label)) then begin
      (* ClPrinter.interpPrint (Atom (ru).label); Printf.printf " "; *)
      (* ClPrinter.interpPrint (Atom (rv).label); Printf.printf " \n"; *)
      ru.parent <- rv;
      ru.parents <- ru::rv.parents;
      (ru,rv)::m
    end else m

let get_eq m u =
  List.assoc u m

let fst_prj l =   
  fst(List.split l)
  
let snd_prj l =   
  snd(List.split l)
  
let selectN m elt =
  snd_prj (List.filter (fun (n1,n2) -> eq_term n1.label elt.label) m)
  
let select m elt =
  snd_prj (List.filter (fun (n1,n2) -> eq_term n1.label elt) m)


  
let congruent m u v = Printf.printf " Enter \n";
  (List.length u.childs) = (List.length v.childs) &&
    List.for_all2 (fun x y ->
        let n1,m = (find m x) in
        let n2,m = (find m y) in
        (* ClPrinter.interpPrint (Atom x.label); Printf.printf " root "; *)
        (* ClPrinter.interpPrint (Atom n1.label); Printf.printf " -- "; *)
        (* ClPrinter.interpPrint (Atom y.label); Printf.printf " root "; *)
        (* ClPrinter.interpPrint (Atom n2.label); Printf.printf " \n "; *)
     eq_term n1.label n2.label)
    u.childs v.childs 
    
let rec merge m u v =
  let ru,m = (find m u) in
  let rv,m = (find m v) in
  (* ClPrinter.interpPrint (Atom (ru).label); Printf.printf " "; *)
  (* ClPrinter.interpPrint (Atom (rv).label); Printf.printf " \n"; *)
  if  not (eq_term (ru.label) (rv.label) ) then begin 
    let eqToU = selectN m u in
    let eqToV = selectN m v in
    (* we construct Pu (resp Pv) the set of parents 
       equivalents node of u (resp v) *)Printf.printf " avant ";
    ClPrinter.interpPrint (Atom (u).label); Printf.printf " ";
    ClPrinter.interpPrint (Atom (v).label); Printf.printf " \n";
    let m = union m ru rv in
    let pu = List.fold_right (fun elt l -> elt.parents@l) eqToU [] in
    let pv = List.fold_right (fun elt l -> elt.parents@l) eqToV [] in
    
    (* List.iter (fun prt -> *)
    (*     ClPrinter.interpPrint (Atom prt.label);Printf.printf " \n";) pu; *)
    
    
    List.fold_right ( fun x tmp1 ->
     let nlst =                  
     List.fold_right ( fun y tmp2 ->
       let rx,m = (find m x) in
       let ry,m = (find m y) in                                 
      if  not (eq_term (rx.label) (ry.label) ) && (congruent m x y) then
        let nlst2 = merge tmp2 x y in
        nlst2@tmp2
      else tmp2
     ) pv tmp1 in nlst@tmp1
    ) pu m
    end else m

let rec go_throw n = 
  let childs = n.parent in
  ClPrinter.interpPrint (Atom n.label);Printf.printf " - "; (* Printf.printf " \n"; *)
  begin
    if  not(eq_term n.parent.label n.label) then  go_throw childs;
  end

let rec is_child question n  = 
  let childs = n.childs in
  List.exists (fun x -> (eq_term x.label question.label) ) childs

 let rec direct_parents question parents  = 
   List.find (fun x -> (eq_term x.label question.label) ) parents

let rec build_child_withP nparents = function
  | Id x ->
     let rec node = {
       label = (Id x);
       childs = [];
       parents = [];
       parent = nparents; } in
     (* Printf.printf " In Go throw "; ClPrinter.interpPrint (Atom (Id x)); Printf.printf " \n"; *)
     (* go_throw node; *)
     node 
    
  | App(a,b) ->
     let rec app = {
         label = App(a,b);
         childs = [];
         parents = [];
         parent = nparents; 
       } in 
     app.childs <- List.map (build_child_withP app) b;
     app    
  | _ -> failwith "term doesn't contain equality"

let build_child_withoutP = function
  | Id x ->
     let rec node = {
       label = (Id x);
       childs = [];
       parents = [];
       parent = node; } in node 
    
  | App(a,b) ->
     let rec app = {
         label = App(a,b);
         childs = [];
         parents = [];
         parent = app; 
       } in 
     app.childs <- List.map (build_child_withP app) b;
     app    
  | _ -> failwith "term doesn't contain equality"
       
         
let make_node t =
  let node = build_child_withoutP t in
  node
  
let rec add_parents lst node =
  (* Printf.printf " In Go throw "; *)
  (* ClPrinter.interpPrint (Atom (node.label)); Printf.printf " \n"; *) 
  
  let parents = List.filter (is_child node) lst in
  (* cleaning step *)
  let parents = elim_bdl node parents in 
  (* Printf.printf " My parents : "; *)
  (* List.iter (fun (node) -> *)
  (*     Printf.printf " "; *)
  (*     ClPrinter.interpPrint (Atom (node.label)); *)
  (*     go_throw node;  ) parents;  Printf.printf " \n"; *)
  try node.parent <- List.nth parents 0 with e -> ();
  node.parents <- node.parents @ parents;  
  List.iter (add_parents lst) (node.childs)
  
  
let congrClosure listeq listerm listneq =
  let index = List.map (fun t ->
                  let n = make_node t in 
                  (t,n) ) listerm in
  List.iter (fun (trm,node) ->
      add_parents (snd_prj index) node) index;
  (* List.iter (fun (trm,node) -> *)
  (*     Printf.printf " In Go throw "; *)
  (*     ClPrinter.interpPrint (Atom (node.label)); Printf.printf " \n"; *)
  (*     go_throw node;  ) index; *)
  (* let req = List.map (fun (t1,t2) -> (assoc t1 index,assoc t2 index) ) listeq in *)
  let rid = List.map (fun t -> (assoc t index,assoc t index) ) listerm in
  let r   = elim_bdl_paires rid in
  
  let cc = elim_bdl_paires (List.fold_left (fun nodelst (t1,t2) ->
               (* Printf.printf "\nt1  : "; ClPrinter.interpPrint (Atom (t1));Printf.printf "  \n"; *)
               (* Printf.printf "t2  : "; ClPrinter.interpPrint (Atom (t2));Printf.printf "  \n"; *)
               (* Printf.printf "merge list :  \n"; *)
               (* List.iter (fun (x,y) ->Printf.printf " (";ClPrinter.interpPrint (Atom (x.label)); Printf.printf ","; *)
               (*     ClPrinter.interpPrint (Atom (y.label)); Printf.printf ") ";) nodelst; *)
               merge nodelst (assoc t1 index) (assoc t2 index)
                              ) r listeq) in
  (* Printf.printf "merge list :  \n"; *)
  (* List.iter (fun (x,y) -> *)
  (*     Printf.printf " (";ClPrinter.interpPrint (Atom (x.label)); Printf.printf ","; *)
  (*     ClPrinter.interpPrint (Atom (y.label)); Printf.printf ") ";) cc; *)
  (* Printf.printf " taille eq %d \n" (List.length listeq); *)
  (* Printf.printf " taille inequation %d \n" (List.length listneq); *)
  
  List.for_all (fun (x,y) ->
      try 
        let equivalentsX = select cc x in
        let equivalentsY = select cc y in
        Printf.printf "x  : "; ClPrinter.interpPrint (Atom (x));Printf.printf "  \n";
        Printf.printf "equive X :  \n";
        List.iter (fun x -> ClPrinter.interpPrint (Atom (x.label)); Printf.printf " ";) equivalentsX;
        Printf.printf " \n";
        Printf.printf "y  : "; ClPrinter.interpPrint (Atom (y));Printf.printf "  \n";
        Printf.printf "equive Y :  \n";
        List.iter (fun x -> ClPrinter.interpPrint (Atom (x.label)); Printf.printf " ";) equivalentsY;
        Printf.printf " \n";
        List.length (inter equivalentsY equivalentsX) = 0
        (* not(mem x equivalentsY) && not(mem y equivalentsX)  *)
      with e -> true
    )  listneq


















  
