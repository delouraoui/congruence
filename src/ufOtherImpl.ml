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

let rec make_node pred = function
  | Id x ->
     cmp := !cmp + 1;  
     [{id = !cmp; fn = x ; args = [];  find = !cmp; ccpar = IdSet.of_list pred }]
     
  | App(Id a,b) ->
     cmp := !cmp + 1;
     {id = !cmp; fn = a ; args = [];  find = !cmp; ccpar = IdSet.of_list pred } ::
       List.flatten (List.map (make_node (!cmp::pred)) b)
     
  | _ -> failwith "term doesn't contain equality"
          
let rec find i lstOfNode =
  let n = List.nth lstOfNode i in
  if (find i lstOfNode) = i then i else (find n.find lstOfNode)

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
    ((List.length n1.args) = (List.length n1.args)) &&
      List.for_all2 (fun arg1 arg2 -> (find arg1 lstOfNode) = (find arg2 lstOfNode) ) n1.args n2.args

let rec merge i1 i2 lstOfNode =
  Printf.printf "salut \n";
  if find i1 lstOfNode <> find i2 lstOfNode then begin
      let pi1 = ccpar i1 lstOfNode in 
      let pi2 = ccpar i2 lstOfNode in 
      union i1 i2 lstOfNode;
      List.iter (
          fun t1 ->
          List.iter (
              fun  t2 ->
              if (find t1 lstOfNode) <> (find t2 lstOfNode) && congruent t1 t2 lstOfNode then
                merge i1 i2 lstOfNode
            ) (IdSet.elements pi2)
        ) (IdSet.elements pi1)
    end

  
let rec eq_term e1 e2 =
  match e1, e2 with
  | Id a',  Id b' -> String.equal a' b'
  | App(Id a ,b), App(Id a',b') ->
     String.equal a a' && (List.for_all2 eq_term b b')
  | Eq(a,b),Eq(a',b') -> 
     eq_term a a' && eq_term b b'
  | _,_ -> false
         
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

let decision sf eqlst neqlst = 
  let index = List.map (fun t -> let n = make_node [] t in (t,n)) sf in
  let lstOfNode = List.flatten (snd (List.split index)) in
  let eqlst = List.map (fun (si,ti) -> (assoc si index,assoc ti index) ) eqlst in
  let neqlst = List.map (fun (si,ti) -> (assoc si index,assoc ti index) ) neqlst in
  List.iter (fun (si,ti) -> merge si ti lstOfNode ) eqlst;
  (* (List.exists (fun (si,ti) -> (find si lstOfNode)  = (find ti lstOfNode) ) neqlst) *)
    assert false
