type 'a tree = 
    | Empty
    | Node of 'a * 'a tree *'a tree

let rec string_of_binary_tree string_of_a tree =
 match tree with 
 | Empty -> "()"
 | Node (a, left, right) -> "(" ^ (string_of_binary_tree string_of_a left) ^ (string_of_a a) ^ (string_of_binary_tree string_of_a right) ^ ")"

let rec search tree key = 
 match tree with 
 | Empty -> false
 | Node (a, left, right) -> if (a = key) then true else 
                            if (a < key) then search right key else search left key

let rec insert tree key =
 match tree with 
 | Empty -> Node (key, Empty, Empty)
 | Node (a, left, right) -> if (a = key) then tree else 
                            if (a < key) then Node(a, left, insert right key) else Node(a, insert left key, right)

let rec min tree = 
 match tree with 
 | Empty -> Empty
 | Node (_, left, _) -> if left = Empty then tree else min left

let rec delete tree key =
 match tree with 
 | Empty -> tree
 | Node (a, left, right) -> if (a = key) then 
                                if left = Empty then right else
                                if right =Empty then left else let temp = min right in match temp with Empty -> tree | Node(a_temp, _, _) -> Node(a_temp, left, delete right a_temp) else
                            if (a < key) then Node (a, left, delete right key) else Node (a, delete left key, right)

