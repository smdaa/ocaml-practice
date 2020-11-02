(* type 'a tree = Empty | Node of 'a * 'a tree * 'a tree *)
type 'a tree = 
    | Empty
    | Node of 'a * 'a tree * 'a tree

(* string_of_binary_tree : ('a -> string) -> 'a tree -> string *)
let rec string_of_binary_tree string_of_a tree =
 match tree with 
 | Empty -> "()"
 | Node (a, left, right) -> "(" ^ (string_of_binary_tree string_of_a left) ^ (string_of_a a) ^ (string_of_binary_tree string_of_a right) ^ ")"

(* find if a key is in a tree     *)
(* search : 'a tree -> 'a -> bool *)
let rec search tree key = 
 match tree with 
 | Empty -> false
 | Node (a, left, right) -> if (a = key) then true else 
                            if (a < key) then search right key else search left key

(* insert a key in a tree            *)
(* insert : 'a tree -> 'a -> 'a tree *)
let rec insert tree key =
 match tree with 
 | Empty -> Node (key, Empty, Empty)
 | Node (a, left, right) -> if (a = key) then tree else 
                            if (a < key) then Node(a, left, insert right key) else Node(a, insert left key, right)

(* return the subtree with the min at the root *)
(* min : 'a tree -> 'a tree *)
let rec min tree = 
 match tree with 
 | Empty -> Empty
 | Node (_, left, _) -> if left = Empty then tree else min left

(* return the subtree with the max at the root *)
(* max : 'a tree -> 'a tree *)
let rec max tree = 
 match tree with 
 | Empty -> Empty
 | Node (_, _, right) -> if right = Empty then tree else max right

(* delete a key from a tree *)
(* delete : 'a tree -> 'a -> 'a tree *)
let rec delete tree key =
 match tree with 
 | Empty -> tree
 | Node (a, left, right) -> if (a = key) then 
                                if left = Empty then right else
                                if right =Empty then left else let temp = min right in match temp with Empty -> tree | Node(a_temp, _, _) -> Node(a_temp, left, delete right a_temp) else
                            if (a < key) then Node (a, left, delete right key) else Node (a, delete left key, right)

(* inorder tree walk *)
(* inorder_tree_walk : 'a tree -> 'a list *)
let rec inorder_tree_walk tree =
 match tree with 
 | Empty -> []
 | Node (a, left, right) -> (inorder_tree_walk left)@[a]@(inorder_tree_walk right)

(* preorder tree walk *)
(* preorder_tree_walk : 'a tree -> 'a list  *)
let rec preorder_tree_walk tree =
 match tree with 
 | Empty -> []
 | Node (a, left, right) -> [a]@(preorder_tree_walk left)@(preorder_tree_walk right)

(* postorder tree walk *)
(* postorder_tree_walk : 'a tree -> 'a list *)
let rec postorder_tree_walk tree =
 match tree with 
 | Empty -> []
 | Node (a, left, right) -> (postorder_tree_walk left)@(postorder_tree_walk right)@[a]

(* find the heigth of a tree *)
(* height : 'a tree -> int *)
let rec height tree = 
 match tree with 
 | Empty -> 0
 | Node (_, left, right) -> let lefth = (height left) and righth = (height right) in (if (lefth < righth) then 1 + righth else 1 + lefth)

(* check if a tree is a binary search tree *)
(* is_a_search_tree : 'a tree -> bool *)
let rec is_a_search_tree tree =
 match tree with 
 | Empty -> true
 | Node (a, left, right) -> match left,right with 
                            | Empty,Empty                        -> true 
                            | Empty,Node(a_right,_,_)            -> if (a > a_right) then false else (is_a_search_tree right)
                            | Node(a_left,_,_),Empty             -> if (a < a_left) then false else (is_a_search_tree left)
                            | Node(a_left,_,_),Node(a_right,_,_) -> if (a > a_right) then false else
                                                                    if (a < a_left) then false else
                                                                    (is_a_search_tree left) && (is_a_search_tree right)


