(* a predicate to test primality *)
let is_prime n = 
 let rec is_prime_aux(n, i) = if (n <= 2) then n = 2 else 
  if (n mod i = 0) then false else 
   if (i * i >n) then true else is_prime_aux(n, i+1)
 in is_prime_aux(n, 2)

let%test _ = is_prime 2 = true
let%test _ = is_prime 1 = false
let%test _ = is_prime 5 = true
let%test _ = is_prime 7 = true
let%test _ = is_prime 18 = false

(* Euclid's algorithm *)
let rec pgcd a b = if b = 0 then a else pgcd b (a mod b)

let%test _ = pgcd 4 2 = 2
let%test _ = pgcd 12 13 = 1
let%test _ = pgcd 6 3 = 3
let%test _ = pgcd 7 12 = 1
let%test _ = pgcd 10 5 = 5

(* two numbers are coprime if their gcd is one *)
let coprime a b = pgcd a b = 1

let%test _ = coprime 7 12 = true
let%test _ = coprime 10 5 = false
let%test _ = coprime 13 12 = true
let%test _ = coprime 6 12 = false

(* Euler's totient function is the number of integers less than m that are coprime to m *)
let phi m = 
 let rec phi_aux(m, i) = if i = m then 0 else 
  if coprime m i then 1 + phi_aux(m, i+1) else phi_aux(m, i+1)
 in phi_aux(m, 0)

let%test _ = phi 1 = 1
let%test _ = phi 2 = 1
let%test _ = phi 3 = 2
let%test _ = phi 4 = 2
let%test _ = phi 5 = 4
let%test _ = phi 6 = 2
let%test _ = phi 7 = 6

(* is p a prime factor of n? It must be both prime and a factor *)
let is_prime_factor p n = 
 is_prime p && n mod p = 0

let%test _ = is_prime_factor 2 6 = true
let%test _ = is_prime_factor 7 77 = true
let%test _ = is_prime_factor 3 5 = false
let%test _ = is_prime_factor 4 11 = false

(* print a list *)
let rec print_list = function 
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l

(* produce the prime factorization of a number as a flat list *)
let factors n =
 let rec factors_aux n i = if (i = 2) then 
        if is_prime_factor i n then [i]   else [] 
    else if is_prime_factor i n then i :: factors_aux n (i-1) else factors_aux n (i-1)
 in factors_aux n (n/2)

let%test _ = factors 330 = [11; 5; 3; 2]
let%test _ = factors 48 = [3; 2]

(* The integer power1 function computes x to the y normal reccursion *)
let rec power1 x y = 
 match x,y with 
 | _,0 -> 1
 | 0,_ -> 0
 | x,y -> x * power1 x (y-1)

let%test _ = power1 0 0 = 1
let%test _ = power1 5 0 = 1
let%test _ = power1 0 5 = 0
let%test _ = power1 2 3 = 8
let%test _ = power1 4 2 = 16

(* The integer power1 function computes x to the y tail reccursion *)
let  power2 x y = 
 let rec power2_aux i acc = if i > y then acc else  power2_aux (i + 1) (acc * x)
 in power2_aux 1 1 

let%test _ = power2 0 0 = 1
let%test _ = power2 5 0 = 1
let%test _ = power2 0 5 = 0
let%test _ = power2 2 3 = 8
let%test _ = power2 4 2 = 16

