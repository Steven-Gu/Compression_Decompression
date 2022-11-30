type tree =
| Leaf of int
| Node of tree * tree


type 'a t = (int * tree) list


let empty = []

let is_singleton h = 
  match h with 
  []-> false
  |_::[] -> true
  |_::_ -> false

let is_empty h = 
  if h = empty then true else false

let add _ _ = failwith "todo"
let find_min _ = failwith "todo"

let remove_min _ = failwith "todo"
