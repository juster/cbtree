type node = Leaf of string | Branch of node * int * node
type t = Empty | Tree of node
type bitstat = On | Off
let empty = Empty

(* Given two strings, find the byte index and the critical bit index.
   The result is packed into an integer, the lower 3 bits being the
   bit index, and the upper 28 or 60 bits containing the byte index. *)
let cbcalc s t =
  let xor s t i = (Char.code s.[i]) lxor (Char.code t.[i]) in
  let rec cbcalc' s t len i =
    if i >= len then None
    else match xor s t i with 0 -> cbcalc' s t len (i+1) | _ as m ->
      (* Performs a log2 of the value of critical bit; lower bits might be 1.
         Reference: http://aggregate.org/MAGIC via agl/critbit *)
      (* Fold upper bits into lower bits so crit bit and below are set. *)
      let m = m lor (m lsr 1) in let m = m lor (m lsr 2) in
      let m = m lor (m lsr 3) in let m = m lor (m lsr 4) in
      (* Count the ones in the byte. *)
      let m = m - ((m lsr 1) land 0x55) in
      let m = ((m lsr 2) land 0x33) + (m land 0x33) in
      let m = ((m lsr 4) + m) land 0x0F in
      (* Pack byte number and critical bit index into a single int *)
      Some ((i lsl 3) lor (m - 1))
  in
  if String.length s = String.length t then cbcalc' s t (String.length s) 0
  else
    let len = min (String.length s) (String.length t) in
    match cbcalc' s t len 0 with None -> Some (len lsl 3) | Some _ as x -> x

(* Test the critical bit specified by the packed cb integer in a string. *)
let cbtest s cb =
  try let c = Char.code s.[cb lsr 3] in
    match c land (1 lsl (cb land 7)) with 0 -> Off | _ -> On
  with Invalid_argument _ -> Off

let mem k cbt =
  let rec walk k = function Leaf k' -> k = k'
  | Branch (left, cb, right) ->
    walk k (match cbtest k cb with On -> right | Off -> left)
  in match cbt with Empty -> false | Tree n -> walk k n

let graft k cb n =
  match cbtest k cb with On -> Branch (n, cb, Leaf k)
  | Off -> Branch (Leaf k, cb, n)

exception Critbit of int

let rec prune k = function
  | Leaf l -> begin
      match cbcalc k l with None -> failwith "key already exists"
      | Some cb -> raise (Critbit cb)
    end
  | Branch (left, cb, right) ->
    let dir = cbtest k cb in
    try prune k (match dir with On -> right | Off -> left)
    with Critbit newcb as e ->
      if newcb < cb then raise e
      else if newcb = cb then failwith "newcb is equal to cb"
      else match dir with On -> Branch (left, cb, graft k newcb right)
      | Off -> Branch (graft k newcb left, cb, right)

let add k cbt =
  match cbt with Empty -> Tree (Leaf k)
  | Tree n -> try Tree (prune k n) with Critbit cb -> Tree (graft k cb n)

exception Foundkey of string

let remove k t =
  let rec walk k = function
    Leaf l -> if k = l then raise (Foundkey l) else failwith "key not found"
  | Branch (left, cb, right) ->
    let dir = cbtest k cb in
    try match dir with On -> Branch (left, cb, walk k right)
      | Off -> Branch (walk k left, cb, right)
    with Foundkey l -> match dir with On -> left | Off -> right
  in match t with Empty -> failwith "key not found"
  | Tree (Leaf l) -> if k = l then Empty else failwith "key not found"
  | Tree (b) -> Tree (walk k b)

let iter ~f t =
  let rec walk f = function Leaf k -> f k
  | Branch (left, _, right) -> walk f left; walk f right
  in match t with Empty -> () | Tree n -> walk f n
