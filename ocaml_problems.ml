(* https://ocaml.org/learn/tutorials/99problems.html *)

(* 1: Write a function last : 'a list -> 'a option that returns the last element of a list (easy) *)
let rec last list =
 match list with 
 | [] -> None
 | h::t -> if (t=[]) then Some h else last t

let%test _ = last [] = None
let%test _ = last [1] = Some 1
let%test _ = last [1;2] = Some 2
let%test _ = last [1;2;3] = Some 3

(* 2: Find the last but one (last and penultimate) elements of a list (easy) *)
let rec last_two list = 
 match list with 
 | [] -> None
 | h1::t1 -> match t1 with 
             | [] -> None
             | h2::t2 -> if (t2 = []) then Some (h1, h2) else last_two t1

let%test _ = last_two [] = None
let%test _ = last_two [1] = None
let%test _ = last_two [1;2] = Some (1, 2)
let%test _ = last_two [1;2;3] = Some (2, 3)

(* 3: Find the k'th element of a list. (easy) *)
let rec at k list =
 match list with 
 | [] -> None
 | h::t -> if (k=1) then Some h else at (k-1) t

let%test _ = at 3 [ "a" ; "b"; "c"; "d"; "e" ] = Some "c"
let%test _ = at 3 [ "a" ] = None
let%test _ = at 0 [] = None

(* 4: Find the number of elements of a list. (easy) *)
let length list =
 List.fold_right (fun _ acc -> 1 + acc) list 0

let%test _ = length [ "a" ; "b" ; "c"] = 3
let%test _ = length [ "a" ; "b" ; "c"] = 3
let%test _ = length [] = 0
                         
(* 5: Reverse a list. (easy) *)
let rec rev list =
 match list with
 | [] -> []
 | h::t -> (rev t)@[h]

let%test _ = rev ["a" ; "b" ; "c"] = ["c"; "b"; "a"]
let%test _ = rev ["a" ; "b"] = ["b"; "a"]
let%test _ = rev [] = []

(* 6: Find out whether a list is a palindrome. (easy) *)
let is_palindrome list = (list = rev list)

let%test _ = is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ] = true
let%test _ = is_palindrome [ "x" ; "a" ; "m" ; "b" ; "x" ] = false

(* 7: Flatten a nested list structure. (medium) *)
type 'a node =
   One of 'a
 | Many of 'a node list

let rec flatten list =
 match list with 
  [] -> []
  | [One a] -> [a]
  | [Many l] -> flatten l
  | h::t ->  match h with 
              One a -> a :: (flatten t)
              | Many b -> (flatten b) @ (flatten t)

let%test _ = flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ] = ["a"; "b"; "c"; "d"; "e"]
let%test _ = flatten [ One "a" ;One "e" ] = ["a"; "e"]

(* 8: Eliminate consecutive duplicates of list elements. (medium) *)
let rec compress list =
 match list with 
 | [] -> []
 | [x] -> [x]
 | h::t -> match t with 
          | [] -> list
          | h1::_ -> if (h = h1) then compress t else h :: compress t

let%test _ = compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = ["a"; "b"; "c"; "a"; "d"; "e"]
let%test _ = compress ["a"; "b"; "c"; "a"; "d"; "e"] = ["a"; "b"; "c"; "a"; "d"; "e"]

(* 10: Run-length encoding of a list. (easy) *)
let encode list =
  let rec aux count acc list= 
    match list with 
    [] -> []
    |[x] -> (count+1, x) :: acc
    | a::(b::_ as t) -> if (a = b) then aux (count + 1) acc t else aux 0 ((count+1, a)::acc) t
  in List.rev (aux 0 [] list)

let%test _ = encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]

(* 14: Duplicate the elements of a list. (easy) *)
let rec duplicate list =
 match list with 
 [] -> []
 | h::t -> h::h::duplicate t

let%test _ = duplicate ["a";"b";"c";"c";"d"] = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]

(* 17: Split a list into two parts; the length of the first part is given. (easy) *)
let rec split list n =
  match list with 
   | [] -> [],[]
   | h::t -> if (n=1) then [h],t else 
              (h::(fst(split t (n-1))), snd(split t (n-1)))

let%test _ = split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
let%test _ = split ["a";"b";"c";"d"] 5 = (["a"; "b"; "c"; "d"], [])

(* 18: Extract a slice from a list. (medium) *)
let rec slice list i k =
 match list with
  | [] -> []
  | h::t -> if (k = 0) then [h] else
            if (i = 0) then h::(slice t i (k-1)) else (slice t (i - 1) (k - 1))

let%test _ = slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6 = ["c"; "d"; "e"; "f"; "g"]

(* 19: Rotate a list N places to the left. (medium) *)
let rotate list n =
  let slice1,slice2 = split list n in slice2@slice1 


let%test _ = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]





