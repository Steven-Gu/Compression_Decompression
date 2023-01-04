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

  let compress f a = 
        let fo = open_in(f) in
        let x = code a in
        let o = open_out(f^"_compressed") in
        let os = of_out_channel(o) in
        let rec combine x s fo = 
          try
            let n = input_byte fo in
            combine x (s^x.(n)) fo
          with End_of_file -> s
        in
        let contenu = combine x "" fo in
        (**String.iter (fun x -> Printf.printf "%d" (int_of_char x - int_of_char '0') )contenu**)
        begin
        String.iter (fun x -> (write_bit os (int_of_char x - int_of_char '0')) )contenu;
        finalize os 
        end
    
  
let decompress f a=
  let fo = open_in f in
  let fs = of_in_channel fo in
  let rec loop fo a =
    let rec loopbis fo a =
        match a with
        |Leaf(i)->Printf.printf "%c" (char_of_int i);
        |Node(left,right)-> 
            let n = read_bit fo in
            if n = 0 then loopbis fo right
              else loopbis fo left
    in
    try
      begin
      loopbis fo a;        
      loop fo a
      end
    with End_of_stream ->Printf.printf"sdfs"
  in
  loop fs a


let test f = 
  let fo = open_in f in 
  let fs = of_in_channel fo in
  let rec loop fo = 
    try 
    let n = read_bit fo in 
    begin
    Printf.printf"%d\n" n;
    loop fo
    end
  with End_of_stream -> Printf.printf"\n"
in
loop fs



    
  

let x = char_freq "freq.txt"
let rec loop h l n = 
  if n = 256 then l
  else if h.(n) != 0 then loop h (add (h.(n),Leaf(n)) l) (n+1)
  else loop h l (n+1)

let a = arbre (loop x [] 0)
(**let () = compress "freq.txt" a**)

(**let() = test "freq.txt_compressed"**)
let () = decompress "freq.txt_compressed" a
let() = Printf.printf"\n"




  