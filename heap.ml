type 'a t = 'a list

exception EmptyListe
let empty = []

let is_singleton h = 
  match h with 
  []-> false
  |_::[] -> true
  |_::_ -> false

let is_empty h = h = empty 

let rec add e h = 
    match h with 
    |[]-> e::[]
    |p::ll-> if (fst e < fst p) then e::p::ll
                else p :: add e ll

let find_min h = 
  if is_empty h then raise EmptyListe
  else List.hd h

let remove_min h = 
  if is_empty h then raise EmptyListe
  else (List.hd h, List.tl h)

  
  
