
module type Regle =
sig
  type tid = int
  type td
  val id : tid
  
  val appliquer : td -> td list
end

module Regle1:Regle with type td = char list =
struct
  type tid = int
  type td = char list
  let id = 1
  let appliquer ch  = 
    match List.rev ch with 
    | 'O'::_ -> [ch @ ['A']]
    | _ -> []
end

module Regle2:Regle with type td = char list = 
struct
  type tid = int 
  type td = char list
  let id = 2
  let appliquer ch = 
    match ch with 
    | 'B'::t -> [ch @ t]
    | _ -> []
end

module Regle3:Regle with type td = char list = 
struct
  type td = char list 
  type tid = int
  let id = 3
  let appliquer ch =
    let rec aux ch acc1 acc2 = 
      match ch with 
      | [] -> acc2
      | 'O'::'O'::'O'::t -> aux ('O'::'O'::t) (acc1@['O']) ((acc1@('A'::t))::acc2)
      | 'A'::'O'::'A'::t -> aux ('O'::'O'::t) (acc1@['A']) ((acc1@('A'::t))::acc2)
      | h::t -> aux t (acc1@[h]) acc2
    in aux ch [] []

end

module Regle4:Regle with type td = char list = 
struct
  type td = char list 
  type tid = int
  let id = 4
  let appliquer ch =
    let rec aux ch acc1 acc2 = 
      match ch with
      | [] -> acc2
      | 'A'::'A'::t -> aux ('A'::t) (acc1@['A']) ((acc1 @ t)::acc2)
      | h::t -> aux t (acc1@[h]) acc2
    in aux ch [] []
end


module type ArbreReecriture =
sig
  type tid = int
  type td
  type arbre_reecriture = 
    | Empty
    | Node of td * (tid * arbre_reecriture) list 

  val creer_noeud : td -> arbre_reecriture

  val racine : arbre_reecriture -> td
  val fils : arbre_reecriture -> (tid * arbre_reecriture) list

  val appartient : td -> arbre_reecriture -> bool
end

module ArbreReecritureBOA:ArbreReecriture with type td = char list =
struct 
  type tid = int
  type td = char list
  type arbre_reecriture = 
    | Empty
    | Node of td * (tid * arbre_reecriture) list 

  let creer_noeud ch = failwith "TODO"
  let racine ar = failwith "TODO"
  let fils ar = failwith "TODO"
  let appartient ch ar = failwith "TODO"
end
