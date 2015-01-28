type node = Leaf of string | Branch of node * int * node
type t = Empty | Tree of node
let empty = Empty

(* Given a key and leaf, find the byte index and the critical bit index.
   The result is packed into an integer. The lower 4 bits are the 1-based bit
   index with zero representing the end of the string. The upper 27 or 59
   bits containing the byte index. *)
let cbcalc k l =
  let xor s t i = (Char.code s.[i]) lxor (Char.code t.[i]) in
    (* XOR two characters from the same index of s and t. *)
  let log2 x =
    (* Performs a log2 of the value of highest bit; lower bits might be 1.
       http://aggregate.org/MAGIC *)
    (* Fold upper bits into lower bits so crit bit and below are set. *)
    let x = x lor (x lsr 1) in let x = x lor (x lsr 2) in
    let x = x lor (x lsr 3) in let x = x lor (x lsr 4) in
    (* Count the ones in the byte. *)
    let x = x - ((x lsr 1) land 0x55) in
    let x = ((x lsr 2) land 0x33) + (x land 0x33) in
    ((x lsr 4) + x) land 0x0F
  in
  let rec cbcalc' s t len i =
    if i >= len then None
    else match xor s t i with 0 -> cbcalc' s t len (i+1) | _ as m ->
      (* Pack byte number and critical bit index into a single int *)
      Some ((i lsl 4) lor (log2 m))
  in let x = String.length k in let y = String.length l in
  match cbcalc' k l (min x y) 0 with Some _ as cb -> cb
  | None ->
    if x < y then Some (x lsl 4)
    else if x > y then Some (y lsl 4)
    else None
  (* A bit index of 0 indicates the premature end of one string. We must
     still compare every byte in the string because we can only provide
     this special case if one string is a prefix of the other. *)

(* Test the string's critical bit, specified by the packed cb integer. *)
let cbtest s cb =
  let i = cb lsr 4 in
  match cb land 0xF with 0 -> if (String.length s) <= i then false else true
  | m ->
    try match (Char.code s.[i]) land m with 0 -> false | _ -> true
    with Invalid_argument _ -> failwith "corrupt cb integer in cbtest"

let mem k cbt =
  let rec walk k = function Leaf k' -> k = k'
  | Branch (left, cb, right) ->
    walk k (match cbtest k cb with true -> right | false -> left)
  in match cbt with Empty -> false | Tree n -> walk k n

let graft k cb n =
  match cbtest k cb with true -> Branch (n, cb, Leaf k)
  | false -> Branch (Leaf k, cb, n)

exception Critbit of int

let rec prune k = function
  | Leaf l -> begin
      match cbcalc k l with None -> failwith "key already exists"
      | Some cb -> raise (Critbit cb)
    end
  | Branch (left, cb, right) ->
    let dir = cbtest k cb in
    try prune k (match dir with true -> right | false -> left)
    with Critbit newcb as e ->
      if newcb < cb then raise e
      else if newcb = cb then failwith "newcb is equal to cb"
      else match dir with true -> Branch (left, cb, graft k newcb right)
      | false -> Branch (graft k newcb left, cb, right)

let add k cbt =
  match cbt with Empty -> Tree (Leaf k)
  | Tree n -> try Tree (prune k n) with Critbit cb -> Tree (graft k cb n)

exception Foundkey of string

let remove k t =
  let rec walk k = function
    Leaf l -> if k = l then raise (Foundkey l) else failwith "key not found"
  | Branch (left, cb, right) ->
    let dir = cbtest k cb in
    try match dir with true -> Branch (left, cb, walk k right)
      | false -> Branch (walk k left, cb, right)
    with Foundkey l -> match dir with true -> left | false -> right
  in match t with Empty -> failwith "key not found"
  | Tree (Leaf l) -> if k = l then Empty else failwith "key not found"
  | Tree (b) -> Tree (walk k b)

let iter ~f t =
  let rec walk f = function Leaf k -> f k
  | Branch (left, _, right) -> walk f left; walk f right
  in match t with Empty -> () | Tree n -> walk f n
