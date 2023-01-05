open Bs
open Heap


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
    let x1,h0 = remove_min h in
    let x2,h1 = remove_min h0 in
    arbre (add ((fst x1) + (fst x2), Node((snd x1), (snd x2))) h1)
    
let code a =
  let x = Array.make 256 "" in
  let l = "" in
  let rec loop a l =
    match a with
    |Leaf(v)-> x.(v) <- l
    |Node(left,right)->
        begin
        loop left (l^"0");
        loop right (l^"1")
      end 
    in
    let () = loop a l in
  x 

let rec sauvegarderArbre a os =
  match a with
  |Leaf(i)-> 
    begin 
      write_bit os 1;
      write_int os i
    end
  |Node(left,right)->
    begin
      write_bit os 0;
      sauvegarderArbre left os;
      sauvegarderArbre right os
    end

let rec lireArbre fo =
  let n = read_bit fo in
  if n = 1 then Leaf(read_int fo)
  else
    let left = lireArbre fo in
    let right = lireArbre fo in 
    Node(left,right)


let compress f = 
  let h = char_freq f in
  let fo = open_in(f) in
  let rec loop h l n = 
    if n = 256 then l
    else if h.(n) != 0 then loop h (add (h.(n),Leaf(n)) l) (n+1)
    else loop h l (n+1)
  in
  let l = loop h [] 0 in
  let a = arbre l in
  let x = code a in
  let o = open_out(f^".hf") in
  let os = of_out_channel(o) in
  let rec loop0 x fo os = 
    try
      let n = input_byte fo in
      String.iter (fun x -> (write_bit os (int_of_char x - int_of_char '0')) )x.(n);
      loop0 x fo os
    with End_of_file ->()
  in
  begin
  sauvegarderArbre a os;
  loop0 x fo os;
  finalize os
  end


  
let decompress f =
  let fo = open_in f in
  let fs = of_in_channel fo in
  let a = lireArbre fs in
  let o = open_out (f^"decompressed") in
  let os = of_out_channel o in
  let rec loop fs os a =
    let rec loopbis fs os a =
        match a with
        |Leaf(i)->write_byte os i;
        |Node(left,right)-> 
          let n = read_bit fs in
          if n = 0 then loopbis fs os left
          else loopbis fs os right
    in
    try
      begin
      loopbis fs os a;
      loop fs os a
      end
    with End_of_stream ->()
  in
  loop fs os a;
  finalize os
