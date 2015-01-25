let print_tree cbt =
  Cbtree.iter ~f:(fun k -> print_endline k) cbt

(*
let strbin s =
  let buf = Buffer.create 8 in
  for i = 0 to (8*(String.length s))-1 do
    (if ((i > 0) && ((i mod 8) = 0)) then Buffer.add_char buf '-');
    let bit = match strbit s i with On -> '1' | Off -> '0' in
    Buffer.add_char buf bit
  done;
  Buffer.to_bytes buf
*)

let _ =
  let tree = Cbtree.add "a" (Cbtree.add "ax" Cbtree.empty) in
  print_tree tree

(*
let _ =
  printf "%s\n" (strbin "ab");
  printf "%s\n" (strbin "AB")

let _ =
  let a = "\xFF\xFF\xFF" in
  let b = "\xF0\x0F\x00" in
  let x = String.length a in
  let c = strblt ~src:a ~dest:b ~op:Xor x in
  for i = 0 to x-1 do
    printf "%02X" (Char.code c.[i])
  done;
  printf "\n"

let _ =
  let a = "\xFF\xF0\x00" in
  let b = "\xFF\xF0" in
  printf "%d\n" (pre_length a b)
*)
  
  
