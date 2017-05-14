open ClAst
       
type eq =
  Equal of term * term

module M = Map.Make (struct type t = int let compare = compare end)  

type t = int M.t

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
