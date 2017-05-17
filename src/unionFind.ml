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
          
     
let empty = []

let rec find u =
  let n1 = u.parent in
  let n2 = u in 
  if eq_term n1.label n2.label then u  (* root *)
  else begin
      let root = find u.parent in
      (* path compression *)
      if not(eq_term root.label u.parent.label) then u.parent <- root;
      root
    end

let union m u v =
  let ru = find u in
  let rv = find v in
  if not (eq_term (ru.label) (rv.label) ) then begin
    rv.parent <- ru;
    rv.parents <- ru::rv.parents;
    (ru,rv)::m
    end else m

let get_eq m u =
  List.assoc u m 
  
let congruent u v =
  (List.length u.childs) <> (List.length v.childs) &&
    List.for_all2 (fun x y ->
        let n1 = (find x) in
        let n2 = (find y) in
    eq_term n1.label n2.label)
    u.childs v.childs 
    
let rec merge m u v =
  let ru = (find u) in
  let rv = (find v) in
  if  not (eq_term (ru.label) (rv.label) ) then begin 
    let eqToU = get_eq m u in
    let eqToV = get_eq m v in

    (* we construct Pu (resp Pv) the set of parents 
       equivalents node of u (resp v) *)
    let pu = eqToU.parents in
    let pv = eqToV.parents in
    let m = union m ru rv in 
    List.fold_right ( fun x tmp1 ->
     List.fold_right ( fun y tmp2 ->
      if  not (eq_term ((find x).label) ((find y).label) ) && (congruent x y) then
        let tmp2 = merge tmp2 x y in
        tmp2
      else tmp2
       ) pv tmp1 
     ) pu m end else m
    
let rec build_child = function
  | Id x ->
     let rec node = {
       label = (Id x);
       childs = [];
       parents = [node];
       parent = node; } in node 
    
  | App(a,b) ->
     let rec app = {
         label = App(a,b);
         childs = List.map build_child b;
         parents = [app];
         parent = app; 
       } in app
          
  | _ -> failwith "term doesn't contain equality"


let make_node t =
  let node = build_child t in
  let rec crossing n =
    let childs = n.childs in
    List.iter
      (fun children ->
        children.parent <- n;
        if eq_term (List.nth children.parents 0).label children.label then
          children.parents <- n::(List.tl children.parents)
        else children.parents <- n::(children.parents);
        crossing children;
      ) childs; in
  crossing node; node

let congrClosure listeq listerm listneq =
  let index = List.map (fun t ->
                  let n = make_node t in 
                  (t,n) ) listerm in
  let req = List.map (fun (t1,t2) -> (assoc t1 index,assoc t2 index) ) listeq in
  let rid = List.map (fun t -> (assoc t index,assoc t index) ) listerm in
  let r   = req@rid in
  let cc = List.fold_left (fun nodelst (t1,t2) ->
               merge r (assoc t1 index) (assoc t2 index)
             ) [] listeq in
  Printf.printf " taille eq %d \n" (List.length listeq);
  Printf.printf " taille inequation %d \n" (List.length listneq);
  List.for_all (fun (x,y) ->
      try 
        let n1 = (find (assoc x index)) in
        let n2 = (find (assoc y index)) in
        ClPrinter.interpPrint (Atom n1.label); Printf.printf " \n";
        ClPrinter.interpPrint (Atom n2.label); Printf.printf " \n";
        not(eq_term n1.label y)
      with e -> true
    )  listneq


















  
