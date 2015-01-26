type node = Leaf of string | Branch of node * int * node
type t = Empty | Tree of node
type bitstat = On | Off
let empty = Empty

(* Given two strings, find the byte index and the critical bit mask.
   The result is packed into an integer, the lower 8 bits being the
   bit mask and the remaining 23 or 55 bits contain the byte index. *)
let cbcalc s t =
  let xor s t i = (Char.code s.[i]) lxor (Char.code t.[i]) in
  let rec cbcalc' s t len i =
    if i >= len then None
    else match xor s t i with 0 -> cbcalc' s t len (i+1) | _ as m ->
      (* Ref http://aggregate.org/MAGIC via agl/critbit *)
      let m = m lor (m lsr 1) in let m = m lor (m lsr 2) in
      let m = m lor (m lsr 3) in let m = m lor (m lsr 4) in
      let m = m land (lnot (m lsr 1)) in
      (* Pack byte number and critical bit mask into a single int *)
      Some ((i lsl 8) lor m)
  in
  if String.length s = String.length t then
    cbcalc' s t (String.length s) 0
  else
    let len = min (String.length s) (String.length t) in
    match cbcalc' s t len 0 with
      None -> Some ((len lsl 8) lor 0x80) | Some _ as x -> x

let cbtest key cb =
  try let c = Char.code key.[cb lsr 8] in
    match c land (cb land 0xFF) with 0 -> Off | _ -> On
  with Invalid_argument _ -> Off

let mem k cbt =
  let rec walk k = function
    Leaf k' -> k = k'
  | Branch (left, cb, right) ->
    walk k (match cbtest k cb with On -> right | Off -> left)
  in match cbt with Empty -> false | Tree n -> walk k n

let graft k cb n =
  match cbtest k cb with
    On -> Branch (n, cb, Leaf k) | Off -> Branch (Leaf k, cb, n)

exception Critbit of int

let rec prune k = function
  | Leaf l ->
    (match cbcalc k l with None -> failwith "key already exists"
     | Some cb -> raise (Critbit cb))
  | Branch (left, cb, right) ->
    let dir = cbtest k cb in
    try prune k (match dir with On -> right | Off -> left)
    with Critbit newcb as e ->
      if newcb < cb then raise e
      else if newcb = cb then failwith "newcb is equal to cb"
      else match dir with On -> Branch (left, cb, graft k newcb right)
      | Off -> Branch (graft k newcb left, cb, right)

let add k cbt =
  match cbt with
    Empty -> Tree (Leaf k)
  | Tree n -> try Tree (prune k n)
    with Critbit cb -> Tree (graft k cb n)

exception Foundkey of string

let remove k t =
  let rec walk k = function
    Leaf l when k = l -> raise (Foundkey l)
  | Leaf _ -> failwith "key not found"
  | Branch (left, cb, right) ->
    let dir = cbtest k cb in
    try match dir with On -> Branch (left, cb, walk k right)
    | Off -> Branch (walk k left, cb, right)
    with Foundkey l -> match dir with On -> left | Off -> right
  in match t with
    Empty -> failwith "key not found"
  | Tree (Leaf l) -> if k = l then Empty else failwith "key not found"
  | Tree (b) -> Tree (walk k b)

let iter ~f cbt =
  let rec walk f = function
    Leaf k -> f k | Branch (left, _, right) -> walk f left; walk f right
  in match cbt with
    Empty -> () | Tree n -> walk f n
