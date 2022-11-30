open Bs
let decompress _ = failwith "todo"
let compress _ = failwith "todo"

let char_freq f =
let x = Array.make 256 0 in
let fo = open_in f in
let rec loop x is =
  try
    let n = input_byte fo in
    let () = x.(n) <- x.(n) + 1 in
    loop x is
  with End_of_file -> x 
in
loop x fo


let x = char_freq "freq.txt" 
let () = Array.iter (Printf.printf"%d ") x
