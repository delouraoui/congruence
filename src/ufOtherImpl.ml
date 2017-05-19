open ClAst
   
let cmp = ref (-1)        
              
type id = int        
                
module IdSet =
  Set.Make( 
      struct
        let compare = Pervasives.compare
        type t = id
      end )
type node = {
    id : id;
    fn : string;
    args : id list;
    mutable find : id;
    mutable ccpar : IdSet.t;
  }
let hash : (ClAst.term * node) list ref = ref []          
let rec eq_term e1 e2 =
  match e1, e2 with
  | Id a',  Id b' -> String.equal a' b'
  | App(Id a ,b), App(Id a',b') ->
     String.equal a a' && (List.for_all2 eq_term b b')
  | Eq(a,b),Eq(a',b') -> 
     eq_term a a' && eq_term b b'
  | _,_ -> false

let rec mem x = function
  | [] -> false
  | x'::q ->
     if eq_term x x' then true
     else mem x q
    
let rec mem_hash x = function
  | [] -> false
  | (x',id)::q ->
     if eq_term x x' then true
     else mem_hash x q
    
let rec elim_bdl = function
  | [] -> []
  | a::q ->
     if mem a q then 
       elim_bdl q
     else a::elim_bdl q
    
let rec mem_paires (x,y) = function
  | [] -> false
  | (x',y')::q ->
     if eq_term x x' && eq_term y y' then true
     else mem_paires (x,y) q
    
let rec elim_bdl_paires = function
  | [] -> []
  | (a,b)::q ->
     if mem_paires (a,b) q then 
       elim_bdl_paires q
     else (a,b)::elim_bdl_paires q
    
let getname = function
  | Id a  -> a
  | App(Id a ,b) -> a
  | _ -> failwith "term doesn't contain equality"
    
let rec assoc x = function
  | [] ->  raise Not_found
  | (x',l)::q ->
     if eq_term x x' then
       (List.find (fun node -> node.fn = (getname x)) l).id
     else assoc x q

    
let rec assocId x = function
  | [] ->  raise Not_found
  | (x',id)::q ->
     if eq_term x x' then
       id
     else assocId x q

    
let rec make_node parents = function
  | Id x ->
     if not(mem_hash (Id x) !hash) then begin
         cmp := !cmp + 1;
         let node = {id = !cmp; fn = x ; args = [];  find = !cmp; ccpar = IdSet.of_list parents } in
         hash := ((Id x),node) :: !hash;
         (* ClPrinter.interpPrint (Atom (Id x)); Printf.printf " id %d \n" !cmp; *)
         [node,!cmp] end
     else [assocId (Id x) !hash,(assocId (Id x) !hash).find ]
  | App(Id a,b) ->
     if not(mem_hash (App(Id a,b)) !hash) then begin
         cmp := !cmp + 1;
         (* ClPrinter.interpPrint (Atom (App(Id a,b))); Printf.printf " id %d \n" !cmp ; *)
         let node = {id = !cmp; fn = a ; args = [];  find = !cmp; ccpar = IdSet.empty } in
         hash := ((App(Id a,b)),node) :: !hash;
         let l = ((List.flatten (List.map (make_node (node.id::parents)) b))) in 
         node.find <- (List.hd (snd (List.split l))) ;
         (node,(List.hd (snd (List.split l)))) :: l
       end else [assocId (App(Id a,b)) !hash, (assocId (App(Id a,b)) !hash).find]
     
  | _ -> failwith "term doesn't contain equality"

       
let rec find i lstOfNode =
  let n = List.nth lstOfNode i in
  if n.find = i then i else (find n.find lstOfNode)

let union i1 i2 lstOfNode =
  let n1 = List.nth lstOfNode i1 in
  let n2 = List.nth lstOfNode i2 in
  n1.find <- n2.find;
  n2.ccpar <- IdSet.union n1.ccpar n2.ccpar;
  n1.ccpar <- IdSet.empty

let ccpar i lstOfNode =
    (List.nth lstOfNode (find i lstOfNode)).ccpar
  
let congruent i1 i2 lstOfNode =
  let n1 = List.nth lstOfNode i1 in
  let n2 = List.nth lstOfNode i2 in
  (String.equal n1.fn n2.fn) &&
    ((List.length n1.args) = (List.length n2.args)) &&
      List.for_all2 (fun arg1 arg2 -> (find arg1 lstOfNode) = (find arg2 lstOfNode) ) n1.args n2.args

let rec merge i1 i2 lstOfNode =
  if find i1 lstOfNode <> find i2 lstOfNode then begin
      let pi1 = ccpar i1 lstOfNode in 
      let pi2 = ccpar i2 lstOfNode in
      Printf.printf "\nnode repa: %d repb: %d: " (find i1 lstOfNode) (find i2 lstOfNode) ;
      Printf.printf "\nnode a: %d b: %d: " i1 i2;
      Printf.printf "\n p1 : ";
      List.iter (fun pi -> Printf.printf " %d " pi;) (IdSet.elements pi1); Printf.printf "\n";
      Printf.printf " p2 : ";
      List.iter (fun pi -> Printf.printf " %d " pi;) (IdSet.elements pi2);
      union i1 i2 lstOfNode;
      List.iter (
          fun t1 ->
          List.iter (
              fun  t2 ->
              if (find t1 lstOfNode) <> (find t2 lstOfNode) && congruent t1 t2 lstOfNode then
                merge t1 t2 lstOfNode
            ) (IdSet.elements pi2)
        ) (IdSet.elements pi1)
    end
  

let decision sf eqlst neqlst =
  (*  Cleaning *)
  let sf = elim_bdl sf in
  let eqlst = elim_bdl_paires eqlst in
  let neqlst = elim_bdl_paires neqlst in
  (* Treatement *)
  let index = List.map (fun t -> let n = fst (List.split (make_node [] t)) in (t,n)) sf in
  let lstOfNode = List.flatten (snd (List.split index)) in
  let eqlst = List.map (fun (si,ti) ->
                  ClPrinter.interpPrint (Atom si); Printf.printf " - ";
                   ClPrinter.interpPrint (Atom ti); Printf.printf " \n" ;
                  (assoc si index,assoc ti index) ) eqlst in 
  let neqlst = List.map (fun (si,ti) ->
                   (assoc si index,assoc ti index) ) neqlst in
  List.iter (fun (si,ti) -> merge si ti lstOfNode ) eqlst;
  (List.for_all (fun (si,ti) -> (find si lstOfNode) <> (find ti lstOfNode) ) neqlst)
