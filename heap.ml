
type 'a t = 'a list

let empty = []

let is_singleton h = 
  match h with 
  []-> false
  |_::[] -> true
  |_::_ -> false

let is_empty h =  h = empty 

let rec add e h = 
  match h with
  |[] -> e::h
  |p::ll -> if compare e p < 0 then e::p::ll
  else p:: add e ll
  
  
let find_min h = 
  List.hd h 

let remove_min h =  
  (List.hd h, List.tl h)


