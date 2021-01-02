open Printf
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

(* 20: Remove the K'th element from a list. (easy) *)
let rec remove_at k list =
 match list with 
  | [] -> []
  | h::t -> if (k=0) then t else h::(remove_at (k-1) t)

let%test _ = remove_at 1 ["a";"b";"c";"d"] = ["a"; "c"; "d"]

(* 21: Insert an element at a given position into a list. (easy) *)
let rec insert_at x k list =
 match list with 
  | [] -> [x]
  | h::t -> if (k=0) then x::h::t else h::(insert_at x (k-1) t)

let%test _ = insert_at "alfa" 1 ["a";"b";"c";"d"] = ["a"; "alfa"; "b"; "c"; "d"]
let%test _ = insert_at "alfa" 3 ["a";"b";"c";"d"] = ["a"; "b"; "c"; "alfa"; "d"]
let%test _ = insert_at "alfa" 4 ["a";"b";"c";"d"] = ["a"; "b"; "c"; "d"; "alfa"]

(* 22: Create a list containing all integers within a given range. (easy) *)
let rec range x y =
 if (x = y) then [x] else
  if (x < y) then x::(range (x+1) y) else x::(range (x-1) y)

let%test _ = range 4 9 = [4; 5; 6; 7; 8; 9]
let%test _ = range 9 4 = [9; 8; 7; 6; 5; 4]

(* 23: Extract a given number of randomly selected elements from a list. (medium) *)
let rec rand_select list n =
 if (n >= length list) then list else 
 match list with 
  | [] -> []
  | h::t -> if (n = 0) then [] else let random = Random.int 2 in 
            if (random = 1) then h::(rand_select t (n-1)) else (rand_select t (n))

let%test _ = length (rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3) = 3
let%test _ = length (rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 5) = 5
let%test _ = rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 20 = ["a";"b";"c";"d";"e";"f";"g";"h"]

(* 24: Lotto: Draw N different random numbers from the set 1..M. (easy) *)
let lotto_select n m = rand_select (range 1 m) n

(* 25: Generate a random permutation of the elements of a list *)
let rec permutation list =
 match list with 
  | [] -> []
  | [x] -> [x]
  | [x; y] -> if (Random.int 2 = 1) then [y; x] else [x; y]
  | h::t -> if (Random.int 2 = 1) then (permutation t)@[h] else h::(permutation t)

(* 26: Generate the combinations of K distinct objects chosen from the N elements of a list. (medium) *)
let rec extract n list =
 if (n <= 0) then [[]] else 
 match list with 
  | [] -> []
  | h::t -> (List.map (fun l -> h :: l) (extract (n-1) t)) @ (extract n t)

let%test _ = extract 2 ["a";"b";"c";"d"] = [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]

(* 95: English number words. (medium) *)
let rec full_words n =
 let num2txt n =
  match n with
   | 0 -> "zero"
   | 1 -> "one"
   | 2 -> "two"
   | 3 -> "three"
   | 4 -> "four"
   | 5 -> "five"
   | 6 -> "six"
   | 7 -> "seven"
   | 8 -> "eight"
   | 9 -> "nine"
   | _ -> failwith "greater than 9" in
 if (n <= 9) then (num2txt n) else String.concat "-" [full_words (n/10); num2txt (n mod 10)]

let%test _ = full_words 175 = "one-seven-five"
let%test _ = full_words 23485 = "two-three-four-eight-five"
let%test _ = full_words 0 = "zero"

(* 96: Syntax checker. (medium) *)
let identifier string =
  if string = "" then false else 
  let rec list_car string = match string with
      | "" -> []
      | string -> (String.get string 0) :: (list_car (String.sub string 1 ( (String.length string ) - 1))) in 
  let list = list_car string in 
  let rec aux list i =
    match list with 
    | [] -> true
    | h::t -> if (h = '-') then 
                if (i = 0) then false else
                match t with 
                 | [] -> false 
                 | h1::_ -> if (h1 = '-') then false else aux t (i+1) else 
              aux t (i+1)
  in aux list 0

let%test _ = identifier "this-is-a-long-identifier" = true
let%test _ = identifier "this-ends-in-" = false
let%test _ = identifier "two--hyphens" = false
let%test _ = identifier "-dash-first" = false
let%test _ = identifier "" = false

(* 97: Sudoku. (medium) *)
module Board = struct
    type t = int array 
    let is_valid c = c >= 1
    let get (b: t) (x, y) = b.(x + y * 9)
    let get_as_string (b: t) pos =
      let i = get b pos in
      if is_valid i then string_of_int i else "."
    let with_val (b: t) (x, y) v =
      let b = Array.copy b in
      b.(x + y * 9) <- v;
      b
    let of_list l : t =
      let b = Array.make 81 0 in
      List.iteri (fun y r -> List.iteri (fun x e ->
        b.(x + y * 9) <- if e >= 0 && e <= 9 then e else 0) r) l;
      b
    let print b =
      for y = 0 to 8 do
        for x = 0 to 8 do
          printf (if x = 0 then "%s" else if x mod 3 = 0 then " | %s"
                  else "  %s")  (get_as_string b (x, y))
        done;
        if y < 8 then
          if y mod 3 = 2 then printf "\n--------+---------+--------\n"
          else printf "\n        |         |        \n"
        else printf "\n"
      done
    let available b (x, y) =
      let avail = Array.make 10 true in
      for i = 0 to 8 do
        avail.(get b (x, i)) <- false;
        avail.(get b (i, y)) <- false;
      done;
      let sq_x = x - x mod 3 and sq_y = y - y mod 3 in
      for x = sq_x to sq_x + 2 do
        for y = sq_y to sq_y + 2 do
          avail.(get b (x, y)) <- false;
        done;
      done;
      let av = ref [] in
      for i = 1 (* not 0 *) to 9 do if avail.(i) then av := i :: !av done;
      !av
    let next (x,y) = if x < 8 then (x+1, y) else (0, y+1)
    (** Try to fill the undecided entries. *)
    let rec fill b ((_,y) as pos) =
      if y > 8 then Some b (* filled all entries *)
      else if is_valid(get b pos) then fill b (next pos)
      else match available b pos with
           | [] -> None (* no solution *)
           | l -> try_values b pos l
    and try_values b pos = function
      | v :: l ->
         (match fill (with_val b pos v) (next pos) with
          | Some _ as res -> res
          | None -> try_values b pos l)
      | [] -> None
end

let sudoku b = match Board.fill b (0,0) with
    | Some b -> b
    | None -> failwith "sudoku: no solution";;

let initial_board =
    Board.of_list [[0; 0; 4;  8; 0; 0;  0; 1; 7];
                   [6; 7; 0;  9; 0; 0;  0; 0; 0];
                   [5; 0; 8;  0; 3; 0;  0; 0; 4];
                   [3; 0; 0;  7; 4; 0;  1; 0; 0];
                   [0; 6; 9;  0; 0; 0;  7; 8; 0];
                   [0; 0; 1;  0; 6; 9;  0; 0; 5];
                   [1; 0; 0;  0; 8; 0;  3; 0; 6];
                   [0; 0; 0;  0; 0; 6;  0; 9; 1];
                   [2; 4; 0;  0; 0; 1;  5; 0; 0]];;

(* http://wwwperso.insa-toulouse.fr/~lebotlan/Y/OCaml/exercises2.html *)
let rec switch l =
  match l with 
  | [] -> []
  | [x] -> [x]
  | h1::h2::t -> h2::h1::(switch t)

let%test _ = switch [] = []
let%test _ = switch [1] = [1]
let%test _ = switch [1;2] = [2;1]
let%test _ = switch [1;2;3] = [2;1;3]
let%test _ = switch [1;2;3;4] = [2;1;4;3]

let rec unpair l = 
  match l with 
  | [] -> []
  | (h1,h2)::t -> h1::h2::(unpair t)

let unpair2 l = List.fold_right (fun (h1, h2) acc -> h1::h2::acc) l []

let%test _ = unpair2 [] = []
let%test _ = unpair [] = []
let%test _ = unpair2 [(1,2)] = [1;2]
let%test _ = unpair [(1,2)] = [1;2]
let%test _ = unpair2 [(1,2);(3,4)] = [1;2;3;4]
let%test _ = unpair [(1,2);(3,4)] = [1;2;3;4]

let rec remove_succ l = 
  match l with 
  | [] -> []
  | [x] -> [x]
  | h1::h2::t -> if h2 = h1 + 1 then (remove_succ (h2::t)) else (h1::(remove_succ (h2::t)))

let%test _ = remove_succ [ 10 ; 20 ; 21 ; 22 ; 23 ; 24; 100 ; 101 ; 110 ] = [10; 24; 101; 110]
let%test _ = remove_succ [ 20 ; 21 ; 22 ; 21 ; 22 ; 23 ] = [22; 23]

let rec combine l1 l2 = 
  match l1,l2 with 
  | [],[] -> []
  | [],_ -> failwith "..."
  | _,[] -> failwith "..."
  | h1::t1,h2::t2 -> (h1,h2)::(combine t1 t2)

let rec keep l1 l2 = 
  match l1,l2 with 
  | [],[] -> []
  | [],_ -> failwith "..."
  | _,[] -> failwith "..."
  | h1::t1,h2::t2 -> if h2 then h1::(keep t1 t2) else (keep t1 t2)

let rec map2 f l1 l2 = 
  match l1,l2 with 
  | [],[] -> []
  | [],_ -> failwith "..."
  | _,[] -> failwith "..."
  | h1::t1,h2::t2 -> (f h1 h2)::(map2 f t1 t2) 

let rec interleave l1 l2 = 
  match l1,l2 with 
  | [],[] -> []
  | _,[] -> l1
  | [],_ -> l2
  | h1::t1,h2::t2 -> h1::h2::(interleave t1 t2)

type bool3 = BTrue | BFalse | Unknown

let and3 b1 b2 = 
  match b1,b2 with 
  | BTrue,BTrue -> BTrue
  | BTrue,Unknown -> Unknown
  | _ -> BFalse

let not3 b = 
  match b with 
  | BTrue -> BFalse
  | BFalse -> BTrue
  | Unknown -> Unknown

type instruction = Plus of int | Mul of int

let apply_instruction n i = 
  match i with 
  | Plus a -> n + a
  | Mul a -> n * a

let rec apply_instructions n li = 
  match li with 
  | [] -> n
  | h::t -> apply_instructions (apply_instruction n h) t

let to_funi i = 
  match i with 
  | Plus a -> (fun x -> a + x)
  | Mul a -> (fun x -> a * x)

let to_funlist li = List.map to_funi li

let rec compact li = 
  match li with 
  | [] -> []
  | [x] -> [x]
  | h1::h2::t -> 
    begin 
      match h1,h2 with 
      | Plus a1,Plus a2 -> (Plus (a1 + a2))::(compact t)
      | Mul a1,Mul a2 -> (Mul (a1 * a2))::(compact t)
      | _ -> h1::(compact (h2::t))
    end

let to_string_i s i = 
  match i with 
  | Plus a -> "(" ^ s ^ " + " ^ (string_of_int a) ^ ")"
  | Mul a -> "(" ^s ^ " * " ^ (string_of_int a) ^ ")"

let rec to_string s li = 
  match li with 
  | [] -> s
  | h::t -> to_string (to_string_i s h) t

type 'a element = Single of 'a | Pair of 'a * 'a
type 'a fonct = One_arg of ('a -> 'a) | Two_args of ('a -> 'a -> 'a)

let taille_element e = 
  match e with 
  | Single _ -> 1
  | Pair _ -> 2

let count_elements le = List.fold_right (fun e acc -> acc + taille_element e) le 0

let apply f e =
  match f e with 
  | One_arg f1, Single a -> f1 a
  | Two_args f2, Pair (a, b) -> f2 a b
  | _,_ -> failwith "..."

(* exam 2k18 *)

type 'a app_list = Nil | Append of 'a app_list * 'a app_list | Cons of 'a * 'a app_list

let rec map_app f l =
  match l with 
  | Nil -> Nil
  | Cons (a, l1) -> Cons (f a, map_app f l1)
  | Append(l1, l2) -> Append(map_app f l1, map_app f l2)

let rec rev_app l =
  match l with 
  | Nil -> Nil
  | Cons(h, t) -> Append(rev_app t, Cons(h, Nil))
  | Append(l, r) -> Append(rev_app r,rev_app l)

let rec fold_right_app f l e = 
  match l with 
  | Nil -> e
  | Cons(h, t) -> f h (fold_right_app f t e)
  | Append(l,r) -> fold_right_app f l (fold_right_app f r e)

let rec to_list al = 
  match al with 
  | Nil ->  []
  | Cons(h,t) -> h::(to_list t)
  | Append(l, r) -> (to_list l) @ (to_list r)

let rec to_list_aux al acc = 
  match al with 
  | Nil -> acc
  | Cons(h, t) -> h::(to_list_aux t acc)
  | Append(l,r) -> to_list_aux l (to_list_aux r acc)


(* https://perso.crans.org/besson/teach/agreg-2017/TP_Programmation_2017-18/TP2__OCaml.pdf *)

let taille l = List.fold_right (fun _ acc -> 1 + acc) l 0

let rec concatene l1 l2 = 
  match l1 with 
  | [] -> l2 
  | h1::t1 -> h1 :: (concatene t1 l2)

let rec appartient x l = 
  match l with 
  | [] -> false 
  | h::t -> if x=h then true else appartient x t

let miroir l =
  let rec aux l acc = 
    match l with 
    | [] -> acc 
    | h::t -> aux t (h::acc)
  in aux l []

let rec alterne l1 l2 = 
  match l1 with 
  | [] -> l2
  | h1::t1 -> h1::(alterne l2 t1)

let nb_occurrences n l =
  let rec aux n l acc =
    match l with 
    | [] -> acc
    | h::t -> if h=n then aux n t (acc+1) else aux n t acc
  in aux n l 0

let nb_occurrences_1 n l = List.fold_right (fun h acc -> if h = n then acc+1 else acc) l 0

let pairs l = 
  let rec aux l acc= 
    match l with 
    | [] -> acc
    | h::t -> if (h mod 2 = 0) then aux t (acc @ [h]) else aux t acc
  in aux l []

let range n =
  let rec aux n acc =
    match n with 
    | 0 -> acc
    | _ -> aux (n-1) acc @ [n]
  in aux n []

let range2 a b s =
  let rec aux a b s acc = 
    if a = b then acc
    else if a > b then failwith "..."
    else if (a+s) > b then acc @ [a]
    else aux (a+s) b s (acc @ [a])
  in aux a b s []

let applique f l = 
  let rec aux f l acc = 
    match l with 
    | [] -> acc
    | h::t -> aux f t (acc @ [(f h)])
  in aux f l []

let rec itere f l =
  match l with 
  | [] -> ()
  | h::t -> let () = f h in itere f t 

let rec qqsoit f l =
  match l with 
  | [] -> true 
  | h::t -> (f h) && (qqsoit f t)

let rec ilexiste f l =
  match l with 
  | [] -> false 
  | h::t -> (f h) || (ilexiste f t)

let filtre f l =
  let rec aux f l acc =
    match l with 
    | [] -> acc 
    | h::t -> if f h then aux f t (acc @ [h]) else aux f t acc 
  in aux f l []

type 'a arbre_bin0 =  Feuille0 of 'a | Noeud0 of ('a arbre_bin0) * 'a * ('a arbre_bin0)

let rec arbre_complet_entier n =
  match n with 
  | n when n < 2 -> Feuille0 0
  | n -> Noeud0 (arbre_complet_entier (n / 2), n, arbre_complet_entier (n / 2))

type arbre_bin = Feuille | Noeud of arbre_bin * arbre_bin;;

let arbre_test = Noeud (Noeud (Noeud (Feuille, Feuille), Feuille), Feuille);;

let rec taille ar = 
  match ar with 
  | Feuille -> 1
  | Noeud(ar1, ar2) -> 1 + (taille ar1) + (taille ar2) 
 