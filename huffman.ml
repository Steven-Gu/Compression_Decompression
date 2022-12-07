open Bs
open Heap
let decompress _ = failwith "todo"
let compress _ = failwith "todo"

type tree =
| Leaf of int
| Node of tree * tree


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
    if is_singleton h then snd (List.hd h)
    else 
      let x1 = fst (remove_min h) in
      let x2 = fst (remove_min (snd (remove_min h))) in
      let h0 = snd (remove_min (snd (remove_min h))) in
      arbre (add ((fst x1) + (fst x2), Node((snd x1), (snd x2))) h0)
      
  let code a =
    let x = Array.make 256 "" in
    let l = "" in
    let rec loop a l =
      match a with
      |Leaf(v)-> x.(v) <- l
      |Node(left,right)->
        let () = loop left ("0"^l) in loop right ("1"^l) 
      in
      let () = loop a l in
      x 
  
  
  let x = char_freq "freq.txt" 
  let () = Array.iter (Printf.printf"%d ") x
  