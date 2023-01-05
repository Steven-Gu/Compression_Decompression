
let usage_msg = "huff [-d] <fichier1> <fichier2> ...\nCompression sans -d et Décompression avec -d"

let d = ref false

let input_files = ref []

let anon_fun filename = input_files := filename :: !input_files


let speclist =
  [("-d", Arg.Set d ,"Décompression du fichier à faire!")]
let main () = 
  begin
  Arg.parse speclist anon_fun usage_msg;
  let rec loop fl f =
    match fl with
    |[] ->()
    |n::fll->f n ; loop fll f 
  in
  if !d then loop !input_files Huffman.decompress
  else loop !input_files Huffman.compress
  end


let () = main ()