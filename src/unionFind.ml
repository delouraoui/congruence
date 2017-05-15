open ClAst
       
type eq =
  Equal of term * term
let cmp x y = if Cc.eq_term x y then 0 else 1
module M = Map.Make (struct type t = term let compare = cmp end)  

type t = term M.t

let not_eq u v = u <> v
             
let empty = M.empty

let find m u = 
  let rec lookup u =
    try lookup (M.find u m) 
    with Not_found -> u
  in lookup u

let union m u v =
  let ru = find m u in
  let rv = find m v in
  if not_eq ru rv then M.add ru rv m
  else m 


let rec merge m u v =
  if Cc.eq_term (find m u) (find m v) then m
  else 
    let un = union m u v in
    assert false
let congrClosure = assert false
  
