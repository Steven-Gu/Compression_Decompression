let usage_msg = "huff.exe [-d] <fichier1> <fichier2> ...\nCompression sans -d et Décompression avec -d"

let d = ref false

let input_files = ref []

let anon_fun filename = input_files := filename :: !input_files


let speclist =
  [("-d", Arg.Set d ,"Décompression du fichier")]
let main () = 
  begin
  Arg.parse speclist anon_fun usage_msg;
  let rec loop fl f =
    match fl with
    |[] ->()
    |n::fll->f n ; loop fll f 
  in
  if !d then begin loop !input_files Huffman.decompress;Printf.printf "Décompression avec succès!\n" end
  else begin loop !input_files Huffman.compress; Printf.printf "Compression avec succès!\n" end;
  (*Huffman.test "freq.txt.hf";*)
  end


let () = main ()
