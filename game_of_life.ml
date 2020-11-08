let live_cell = 1
let dead_cell = 0
let size_cell = 10

let is_alive cell = cell <> dead_cell

let rules cell n =
  if (cell = dead_cell) && (n = 3) then live_cell else 
  if (n = 3) || (n =2) then cell else dead_cell

let gen_board size x = 
  let rec aux size x =
    if (size = 0) then [] else x::(aux (size -1) x)
  in 
  let rec aux1 list n =
    if (n = 0) then [] else list::(aux1 list (n-1))
  in
  aux1 (aux size x) size

let get_cell (x,y) board =
  let rec extract_row = function
    | (_, []) -> dead_cell
    | (1, e::_) -> extract_cell (y,e)
    | (n, _::l) -> extract_row ((n-1), l)
  and extract_cell = function 
    | (_, []) -> dead_cell
    | (1, e::_) -> e
    | (n, _::l) -> extract_cell ((n-1), l)
  in extract_row (x, board);;

let rec put_cell cell (x,y) board =
  let rec aux x k list =
  match list with 
  | [] -> []
  | h::t -> if (k=1) then x::t else h::aux x (k - 1) t
  in
  match board with
   | [] -> []
   | h::t -> if (x=1) then (aux cell y h)::t else h::(put_cell cell (x-1,y) t)

let count_neighbours (x,y) board = 
  let add (x, y) = if is_alive (get_cell (x,y) board) then 1 else 0 in 
  add (x-1,y-1) + add (x-1,y) + add (x-1,y+1) + add (x,y-1) + add (x,y+1)
  + add (x+1,y-1) + add (x+1,y) + add (x+1,y+1)
