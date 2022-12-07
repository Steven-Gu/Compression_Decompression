open Bs
open Heap

type tree = 
    | Leaf of int
    | Node of tree*tree


let decompress _ = failwith "todo"
let compress _ = failwith "todo"

let char_freq f =
let x = Array.make 256 0 in
let fo = open_in f in
let rec loop x is =
  try
    let n = input_byte is in
    let () = x.(n) <- x.(n) + 1 in
    loop x is
  with End_of_file -> x 
in
loop x fo
 
let rec arbre h = 
  if is_singleton h then snd(List.hd h)
  else 
    let x1 = fst (remove_min h) in
    let x2 = fst (remove_min (snd (remove_min h))) in
    let h0 = snd (remove_min (snd (remove_min h))) in
    arbre (add ((fst x1) + (fst x2),Node((snd x1), (snd x2))) h0) 
    
let compress f = 
  let h = char_freq f in 
  let rec loop h l n = 
    if n = 256 then l
    else if h.(n) != 0 then loop h (add (h.(n),Leaf(n)) l) (n+1)
    else loop h l (n+1)
  in
  let l = loop h [] 0 in
  arbre l


let x = char_freq "freq.txt" 
let () = Array.iter (Printf.printf"%d ") x


  