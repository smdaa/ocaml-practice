type 'a tree

val string_of_binary_tree : ('a -> string) -> 'a tree -> string

val search : 'a tree -> 'a -> bool

val insert : 'a tree -> 'a -> 'a tree

val min : 'a tree -> 'a tree

val max : 'a tree -> 'a tree

val delete : 'a tree -> 'a -> 'a tree

val inorder_tree_walk : 'a tree -> 'a list

val preorder_tree_walk : 'a tree -> 'a list

val postorder_tree_walk : 'a tree -> 'a list

val height : 'a tree -> int

val is_a_search_tree : 'a tree -> bool
