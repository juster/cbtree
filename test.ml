(*
type bltop = Xor
let strblt ~src:s ~dest:d ~op ?(srci=0) ?(desti=0) count =
  let d = Bytes.of_string d in
  for i = 0 to count-1 do
    let x = Char.code s.[srci + i] in
    let y = match op with
      Xor -> x lxor (Char.code d.[desti + i])
    in
    Bytes.set d (desti + i) (Char.chr y)
  done;
  Bytes.to_string d

let _ =
  let a = "\xFF\xFF\xFF" in
  let b = "\xF0\x0F\x00" in
  let x = String.length a in
  let c = strblt ~src:a ~dest:b ~op:Xor x in
  for i = 0 to x-1 do
    printf "%02X" (Char.code c.[i])
  done;
  printf "\n"

*)

let strbits s =
  let rec binstr x m b =
    if m = 0 then Buffer.contents b
    else binstr x (m lsr 1)
      (Buffer.add_char b (if x land m = 0 then '0' else '1'); b)
  in let rec rev_codes s i codes =
    if i >= String.length s then codes
    else rev_codes s (i+1) ((Char.code s.[i]) :: codes)
  in List.rev_map (fun x -> binstr x 0x80 (Buffer.create 8))
    (rev_codes s 0 [])

let strbin s = String.concat "-" (strbits s)

open Printf
let _ =
  printf "%s\n" (strbin "ab");
  printf "%s\n" (strbin "AB")

let print_tree cbt =
  Cbtree.iter ~f:(fun k -> print_endline k) cbt
  
let _ =
  let t = Cbtree.add "ax" Cbtree.empty in
  let t = Cbtree.add "a" t in
  let t = Cbtree.add "b" t in
  print_tree t
