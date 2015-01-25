open Printf
open Printexc

type node = Leaf of string | Branch of node * int * node
type tree = Empty | Tree of node
type bitstat = On | Off

let strbit key bit =
  try
    let c = Char.code key.[bit/8] in
    let b = 0xFF lxor (1 lsl (7-(bit mod 8))) in
    let x = ((c lor b) + 1) lsr 8 in
    match x with 0 -> Off | _ -> On
  with Invalid_argument "index out of bounds" -> Off

let mem k cbt =
  let rec walk k = function
    Leaf k' -> k = k'
  | Branch (left, cbit, right) ->
    match strbit k cbit with
      On -> walk k right | Off -> walk k left
  in
  match cbt with Empty -> false | Tree n -> walk k n

(* Determine the prefix length. *)
exception CritBit of int
let pre_length k k' =
  let get s i = try Char.code s.[i] with Invalid_argument _ -> 0 in
  let prelen s t =
    let slen = String.length s and tlen = String.length t in
    for i = 0 to (max slen tlen)-1 do
      match (get s i) lxor (get t i) with
        0 -> () | x ->
          for j = 0 to 7 do
            match x land (1 lsl (7-j)) with
              0 -> () | _ -> raise (CritBit ((i*8)+j))
          done
    done
  in try prelen k k'; -1 with CritBit i -> i

let add k cbt =
  (* Create a new branch for two keys. *)
  let newbranch k k' len =
    if len = -1 then failwith "identical key found in newbranch"
    else match strbit k len, strbit k' len with
      On, Off -> Branch (Leaf k', len, Leaf k)
    | Off, On -> Branch (Leaf k, len, Leaf k')
    | On, On | Off, Off -> failwith "pre_length gave false result"
  in

  (* Find the left-most leaf in the subtree. *)
  let rec findlen k = function
    Leaf k' -> pre_length k k'
  | Branch (left, _, _) -> findlen k left
  in

  (* Replace tree nodes as we search for our rightful place. *)
  let rec prune k len n =
    if len = -1 then failwith "identical key found in prune"
    else match n with
      (* If we stumble upon a leaf, then split it into a branch. *)
      Leaf k' -> newbranch k k' len
      (* Common prefix with subtree is < active branch. Replace this branch. *)
    | Branch (_, blen, _) as b when len < blen ->
      begin match strbit k len with
        On -> Branch (b, len, Leaf k) | Off -> Branch (Leaf k, len, b)
      end
      (* Otherwise check our critbit to see whether we go left or right. *)
    | Branch (left, blen, right) ->
      begin match strbit k blen with
        (* We already have left subtree's length. *)
        On -> Branch (left, blen, prune k (findlen k right) right)
      | Off -> Branch (prune k len left, blen, right)
      end
  in

  match cbt with
    Empty -> Tree (Leaf k) | Tree n -> Tree (prune k (findlen k n) n)

let traverse cbt =
  let rec walk = function
    Leaf k -> printf "%s\n" k
  | Branch (left, _, right) -> walk left; walk right
  in match cbt with Empty -> () | Tree node -> walk node

let remove k cbt =
  let notfound _ = failwith "key not found" in
  let rec walk k = function
    Leaf _ -> failwith "remove walked into a leaf"
  | Branch (left, len, right) ->
    let cb = strbit k len in
    match cb, left, right with
      Off, Leaf k', _ -> if k = k' then right else notfound ()
    | On, _, Leaf k'  -> if k = k' then left else notfound ()
    | Off, _, _ -> Branch (walk k left, len, right)
    | On, _, _ -> Branch (left, len, walk k right)
  in match cbt with
    Empty -> notfound ()
  | Tree (Leaf k') -> if k = k' then Empty else notfound ()
  | Tree (Branch (_, _, _) as b) -> Tree (walk k b)

let strbin s =
  let buf = Buffer.create 8 in
  for i = 0 to (8*(String.length s))-1 do
    (if ((i > 0) && ((i mod 8) = 0)) then Buffer.add_char buf '-');
    let bit = match strbit s i with On -> '1' | Off -> '0' in
    Buffer.add_char buf bit
  done;
  Buffer.to_bytes buf

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