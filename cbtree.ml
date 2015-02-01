type node = Leaf of string | Branch of node * int * node
type t = Empty | Tree of node

let empty = Empty

(* Given a key and leaf, find the byte index and the critical bit index.
   The result is packed into an integer.

   The lower 4 bits is the 1-based bit index with 0 representing the
   premature end of a string. Bit indices are backwards from
   convention. The MSB is bit index 1, the LSB is bit index 8.

   The upper 27 bits, or 59 bits on 64-bit ocaml, contain the byte index. *)

let cbcalc k l =
  let xor s t i = (Char.code s.[i]) lxor (Char.code t.[i]) in
  let rec bit x i =
    assert ((i > 0) && (i <= 8));
    match x land (1 lsl (8-i)) with
    | 0 -> bit x (i+1)
    | _ -> i
  in
  let rec cbcalc' s t len i =
    if i >= len then None
    else match xor s t i with
    | 0 -> cbcalc' s t len (i+1)
    | m -> Some ((i lsl 4) lor (bit m 1))
  in
  let x = String.length k
  and y = String.length l in
  match cbcalc' k l (min x y) 0 with
  | Some _ as cb -> cb
  | None ->
    if x < y then Some (x lsl 4)
    else if x > y then Some (y lsl 4)
    else None
  (* A bit index of 0 indicates the premature end of one string. We must
     still compare every byte in the string because we can only provide
     this special case if one string is a prefix of the other. *)

type bitdir = Lhs | Rhs | End

(* Test the string's critical bit, specified by the packed cb integer. *)

let cbtest s b =
  let i = b lsr 4 in
  if (String.length s) <= i then End
  else
    match b land 0xF with
    | 0 -> Rhs
    | m ->
      try
        match (Char.code s.[i]) land (0x80 lsr (m-1)) with
        | 0 -> Lhs
        | _ -> Rhs
      with Invalid_argument _ -> assert false

(* Create a critbit for the "premature" end of string s. *)

let cbend s =
  (String.length s) lsl 4

(* Test if key k is present in the given tree. *)

let mem k =
  let rec walk k = function
  | Leaf l -> k = l
  | Branch (left, b, right) ->
    match cbtest k b with
    | Rhs -> walk k right
    | Lhs -> walk k left
    | End -> false
  in
  function
  | Empty -> false
  | Tree n -> walk k n

(* Graft a node on to a new branch with the given leaf as sibling. *)

let graft l n b = function
  | Rhs -> Branch (n, b, Leaf l)
  | Lhs | End -> Branch (Leaf l, b, n)

exception Critbit of int * bitdir

(* Add a key to a non-empty tree in the form of nodes. *)

let rec add' k =
  function
  | Leaf l ->
    begin
      match cbcalc k l with
      | None -> failwith "key already exists"
      | Some cb -> raise (Critbit (cb, cbtest k cb))
    end
  | Branch (l, b, r) ->
    let d = cbtest k b in
    try
      match d with
      | Lhs -> add' k l
      | Rhs -> add' k r
      | End -> raise (Critbit (cbend k, End))
    with Critbit (b', d') as e ->
      assert (b' <> b);
      if b' < b then raise e
      else
        match d with
        | Lhs -> Branch (graft k l b' d', b, r)
        | Rhs -> Branch (l, b, graft k r b' d')
        | End -> assert false

let add k cbt =
  match cbt with
  | Empty -> Tree (Leaf k)
  | Tree n ->
    try Tree (add' k n) with
    | Invalid_argument _ -> Tree (Branch (Leaf k, cbend k, n))
    | Critbit (b, d) -> Tree (graft k n b d)

(* Remove a key from a tree. *)

exception Foundkey

let remove k t =
  let rec walk k = function
  | Leaf l -> if k = l then raise Foundkey else raise Not_found
  | Branch (l, b, r) ->
    let d = cbtest k b in
    try
      match d with
      | Lhs -> Branch (walk k l, b, r)
      | Rhs -> Branch (l, b, walk k r)
      | End -> raise Not_found
    with Foundkey -> match d with Rhs -> l | Lhs -> r | End -> assert false
  in
  match t with
  | Empty -> raise Not_found
  | Tree b -> try Tree (walk k b) with Foundkey -> Empty

(* Iterate through every leaf of the given tree. *)

let iter ~f t =
  let rec walk f = function
  | Leaf k -> f k
  | Branch (left, _, right) -> walk f left; walk f right
  in
  match t with
  | Empty -> ()
  | Tree n -> walk f n

(* Find the leftmost leaf of a given node tree. *)

let rec leftmost = function
  | Leaf l -> l
  | Branch (l,_,_) -> leftmost l

(* Find the node that would be on our RHS if we inserted a new leaf
   using b' and d'. Raise the Critbit exception to backtrack up the
   tree. *)

let rhsnode b d b' d' l r =
  let btrk () = raise (Critbit (b', d')) in
  if b' < b then btrk ()
  else
    match d, d' with
    | Lhs, (Lhs|End) -> leftmost l
    | Lhs, Rhs | Rhs, (Lhs|End) -> leftmost r
    | Rhs, Rhs -> btrk ()
    | End, _ -> assert false

let rec after' k = function
  | Leaf l ->
    begin
      match cbcalc k l with
      | None -> raise Not_found (* search prefix already exists as a key *)
      | Some b -> raise (Critbit (b, cbtest k b))
    end
  | Branch (l, b, r) ->
    let d = cbtest k b in
    try
      match d with
      | Lhs -> after' k l
      | Rhs -> after' k r
        (* The k prefix is shorter than the common prefix of this subtree. *)
      | End -> rhsnode b End (cbend k) Lhs l r
    with
    | Critbit (b', d') ->
      rhsnode b d b' d' l r
    | Not_found ->
      match d with
      | Lhs -> leftmost r
      | Rhs -> raise (Critbit (b, d))
      | End -> assert false

let after k = function
  | Empty -> raise Not_found
  | Tree n ->
    try after' k n with
    | Critbit (_, Rhs) -> raise Not_found
    | Critbit (_, (Lhs|End)) -> leftmost n
