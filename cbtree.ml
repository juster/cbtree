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

(* Test if key k is present in the given tree. *)
let mem k =
  let rec walk k = function Leaf l -> l
  | Branch (left, cb, right) ->
    walk k (match cbtest k cb with true -> right | false -> left)
  in function Empty -> false | Tree n -> k = (walk k n)

(* Add a key to a tree with might be empty. *)

let graft k cb n =
  match cbtest k cb with true -> Branch (n, cb, Leaf k)
  | false -> Branch (Leaf k, cb, n)

exception Critbit of int

(* Add a key to a non-empty tree in the form of nodes. *)

let rec add' k = function
  | Leaf l -> begin
      match cbcalc k l with None -> failwith "key already exists"
      | Some cb -> raise (Critbit cb)
    end
  | Branch (left, cb, right) ->
    let dir = cbtest k cb in
    try add' k (if dir then right else left)
    with Critbit newcb as e ->
      if newcb < cb then raise e
      else if newcb = cb then failwith "newcb is equal to cb"
      else if dir then Branch (left, cb, graft k newcb right)
      else Branch (graft k newcb left, cb, right)

let add k cbt =
  match cbt with Empty -> Tree (Leaf k)
  | Tree n -> try Tree (add' k n) with Critbit cb -> Tree (graft k cb n)

(* Remove a key from a tree. *)

exception Foundkey of string

let remove k t =
  let rec walk k = function
    Leaf l -> if k = l then raise (Foundkey l) else failwith "key not found"
  | Branch (left, cb, right) ->
    let dir = cbtest k cb in
    try if dir then Branch (left, cb, walk k right) else Branch (walk k left, cb, right)
    with Foundkey l -> if dir then left else right
  in match t with Empty -> failwith "key not found"
  | Tree (Leaf l) -> if k = l then Empty else failwith "key not found"
  | Tree (b) -> Tree (walk k b)

(* Iterate through every leaf of the given tree. *)

let iter ~f t =
  let rec walk f = function Leaf k -> f k
  | Branch (left, _, right) -> walk f left; walk f right
  in match t with Empty -> () | Tree n -> walk f n

let after k =
  let rec diveleft = function Leaf l -> l | Branch (left,_,_) -> diveleft left
  in let rec after' k = function
    Leaf l -> failwith "root leaf found in after'" (* root node is a leaf *)
  | Branch (left, cb, right) ->
    let dir = cbtest k cb in
    try match dir, left, right with
      false, Leaf l, _
    | true, _, Leaf l ->
        let newcb = match cbcalc k l with None -> cb | Some x -> x in
        raise (Critbit newcb)
    | false, (Branch (_,_,_) as b), _
    | true, _, (Branch (_,_,_) as b) -> after' k b
    with Critbit newcb as e ->
      if newcb < cb then raise e
      else if dir then raise e (* backtrack until we can dive right *)
      else Some (diveleft right)
  in function Empty -> None
  | Tree (Leaf l) -> begin match cbcalc k l with
      None -> None | Some cb when cbtest k cb -> None | Some _ -> Some l
    end
  | Tree (Branch (_,_,_) as n) -> try after' k n with Critbit cb ->
    if cbtest k cb then None else Some (diveleft n)
