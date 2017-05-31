open ClAst
   
let cmp = ref (-1)        

exception Not_found_node
        
type id = int        
        
module IdSet =
  Set.Make( 
      struct
        let compare = Pervasives.compare
        type t = id
      end )
type node = {
    id : id;
    partial : bool;
    tot: int;
    fn : string;
    args : id list;
    mutable find : id;
    mutable ccpar : IdSet.t;
  }
let hash : (ClAst.term * node) list ref = ref []
let getother : (id * ClAst.term) list ref = ref []                              
let repr : (ClAst.term * node) list ref = ref []
                                        
let rec eq_term e1 e2 =
  match e1, e2 with
  | Id a',  Id b' -> String.equal a' b'
  | App(Id a ,b), App(Id a',b') ->
     String.equal a a' && (List.for_all2 eq_term b b')
  | PApp(applied,tot,Id a,b),PApp(applied',tot',Id a',b') ->
     applied = applied' && tot = tot' &&
     String.equal a a' && (List.for_all2 eq_term b b')
  | Eq(a,b),Eq(a',b') -> 
     eq_term a a' && eq_term b b'
  | _,_ -> false
         
let rec is_sub_term f app e =
  match e with
  | Id a' -> false
  | App(Id a ,b) ->
     String.equal a f
  | PApp(applied,tot,Id a,b) ->
     applied < app && String.equal a f
  | _ -> false
       
let rec mem x = function
  | [] -> false
  | x'::q ->
     if eq_term x x' then true
     else mem x q
    
let rec node id = function
  | [] -> raise Not_found_node
  | n::q ->
     if n.id = id then n
     else node id q
    
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

let rec mem_nd x = function
  | [] -> false
  | x'::q ->
     if String.equal x.fn x'.fn then true
     else mem_nd x q

    
let rec elim_bdl_paires = function
  | [] -> []
  | (a,b)::q ->
     if mem_paires (a,b) q then 
       elim_bdl_paires q
     else (a,b)::elim_bdl_paires q
    
let rec elim_bdl_nd : node list -> node list = function
  | [] -> []
  | a::q ->
     if mem_nd a q then 
       elim_bdl_nd q
     else a::elim_bdl_nd q
    
let getname = function
  | Id a  -> a
  | App(Id a ,b) -> (ClPrinter.to_string (Atom(App(Id a,b))))
  | PApp(applied,tot,Id a,b) -> (ClPrinter.to_string (Atom(PApp(applied,tot,Id a,b)))) 
  | _ ->  failwith "term doesn't contain equality"
       
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
         let node = {id = !cmp;
                     partial = false;
                     tot = 0;
                     fn = x ;
                     args = [];
                     find = !cmp;
                     ccpar = IdSet.of_list parents } in
         hash := ((Id x),node) :: !hash;
         getother := (!cmp,(Id x)) :: !getother;
         [node,!cmp] end
     else [assocId (Id x) !hash,(assocId (Id x) !hash).find ]
  | App(Id a,b) ->
     
     if not(mem_hash (App(Id a,b)) !hash) then begin
         cmp := !cmp + 1;
         let node =
           {id = !cmp;
            partial = false;
            tot = List.length b;
            fn = (ClPrinter.to_string (Atom(App(Id a,b)))) ;
            args = [];
            find = !cmp;
            ccpar = IdSet.empty } in
         hash := ((App(Id a,b)),node) :: !hash;
         getother := (!cmp,(App(Id a,b))):: !getother;
         let l = ((List.flatten (List.map (make_node (node.id::parents)) b))) in
         node.find <- (List.hd (snd (List.split l)));
         (node,(List.hd (snd (List.split l)))) :: l
       end else [assocId (App(Id a,b)) !hash, (assocId (App(Id a,b)) !hash).find]
    
  | PApp(appl,tot,Id a,b) ->
     if not(mem_hash (PApp(appl,tot,Id a,b)) !hash) then begin
         cmp := !cmp + 1;
         let node =
           {id = !cmp;
            partial = true;
            tot = tot;
            fn = (ClPrinter.to_string (Atom(PApp(appl,tot,Id a,b)))) ;
            args = [];
            find = !cmp;
            ccpar = IdSet.empty } in
         hash := ((PApp(appl,tot,Id a,b)),node) :: !hash;
         getother := (!cmp,(PApp(appl,tot,Id a,b))) :: !getother;   
         if List.length b > 0 then begin
           let l = ((List.flatten (List.map (make_node (node.id::parents)) b))) in
           node.find <- (List.hd (snd (List.split l)));  (node,(List.hd (snd (List.split l)))) :: l end
         else begin node.find <- !cmp; [node,!cmp] end 
         
       end else [assocId (PApp(appl,tot,Id a,b)) !hash, (assocId (PApp(appl,tot,Id a,b)) !hash).find]
    
  | _ -> failwith "term doesn't contain equality"

       
let rec find i lstOfNode =
  let n = node i lstOfNode in
  if n.find = i then i else (find n.find lstOfNode)

let union i1 i2 lstOfNode =
  let n1 = node (find i1 lstOfNode) lstOfNode  in
  let n2 = node (find i2 lstOfNode) lstOfNode  in
  n1.find <- n2.find;
  n2.ccpar <- IdSet.union n1.ccpar n2.ccpar;
  n1.ccpar <- IdSet.empty

let ccpar i lstOfNode =
  (node (find i lstOfNode) lstOfNode).ccpar
  
let congruent i1 i2 lstOfNode =
  let n1 = node i1 lstOfNode in
  let n2 = node i2 lstOfNode in
  (String.equal n1.fn n2.fn) &&
    ((List.length n1.args) = (List.length n2.args)) &&
      List.for_all2 (fun arg1 arg2 -> (find arg1 lstOfNode) = (find arg2 lstOfNode) ) n1.args n2.args

let get_sub_child u (lstOfNode : node list) =
 
  match (List.assoc u (!getother)) with
  |  PApp(appl,tot,Id f,b) -> 
     List.map (fun n -> n.id)
              (List.filter (
                   fun x ->
                   is_sub_term f appl (List.assoc x.id (!getother)) 
                 ) lstOfNode )
  | _ -> failwith "isn't a function then it can't be a partial application"
  
let rec merge i1 i2 lstOfNode =
  if find i1 lstOfNode <> find i2 lstOfNode then begin
      (* HO Part *)
      let n1 = node i1 lstOfNode in
      let n2 = node i2 lstOfNode in
      if n1.partial && n2.partial then begin
          
          let culst = get_sub_child i1 lstOfNode in
          let cvlst = get_sub_child i2 lstOfNode in
          List.iter (fun cu -> union i1 cu lstOfNode) culst;
          List.iter (fun cu -> union i2 cu lstOfNode) cvlst;
        end else () ;
      (* HO Part *)
      let pi1 = ccpar i1 lstOfNode in 
      let pi2 = ccpar i2 lstOfNode in
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
  let lstOfNode = elim_bdl_nd lstOfNode in
  let eqlst = List.map (fun (si,ti) -> (assoc si index,assoc ti index) ) eqlst in
  let neqlst = List.map (fun (si,ti) -> (assoc si index,assoc ti index) ) neqlst in
  List.iter (fun (si,ti) -> merge si ti lstOfNode; ) eqlst;
  (List.for_all (fun (si,ti) ->
       (find si lstOfNode) <> (find ti lstOfNode) ) neqlst)
