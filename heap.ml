type tree =
| Leaf of int
| Node of tree * tree


type 'a t = (int * tree) list

exception EmptyListe
let empty = []

let is_singleton h = 
  match h with 
  []-> false
  |_::[] -> true
  |_::_ -> false

let is_empty h = 
  if h = empty then true else false

let add e h = 
  e::h
  
let find_min h = 
  let cmp h1 h2 =
    if fst h1 < fst h2 then -1
    else if fst h1 = fst h2 then 0
    else 1
  in
  match List.sort cmp h with
  []-> raise EmptyListe
  |e::_ ->e

let remove_min h =  
  let e = try find_min h with EmptyListe-> raise EmptyListe in
  (e , List.filter (fun x -> x != e) h )


  
