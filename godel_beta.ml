(* Godel's Beta-function in OCaml, to see how it really works *)
(* Shane Steinert-Threlkeld, 06/16/2010 *)

(* This is what the beta function does, but provides no insight. *)
(* One can look at implementation of List.nth to see if it uses Godel's beta to calculate. *)
let beta_ocaml (seq:int list) (i:int) = List.nth seq i;;

(* HELPERS; these are not explicitly mathematical *)

(* This is Cantor's classic function from N*N -> N *)
let pairing ((a:int), (b:int)) = int_of_float (0.5 *. float_of_int ((a + b) * (a+b) + 3*a + b));;

(* Inverse of it; after all, Cantor showed that N*N and N are equinumerous, i.e. the pairing function is 1-1 and onto *)
let inverse_pairing (z:int) =
	let w = int_of_float (floor (((sqrt (float_of_int (8*z + 1))) -. 1.0) /. 2.0))
	in let t = int_of_float (((float_of_int (w*w + w)) /. 2.0))
		in let x = z - t in
		(x,w-x);;

(* While I know that these projection functions will only be used on integers, I'm defining them with polymorphic types for maximum generality. *)
let proj1 (x,y) = x;;
let proj2 (x,y) = y;;

(* Generic factorial algorithm *)
let rec factorial (n:int) =
	if n == 0 then 1 else n * factorial (n-1);;

(* List a ... b *)
let rec range a b =
	if a > b then []
	else a :: range (a+1) b;;

(* Calculates the maximum value of an integer list *)
let max (intlist:int list) = List.fold_left (function x -> function y -> if x > y then x else y) 0 intlist;;

(* product of a list of integers *)
let product (intlist:int list) = List.fold_left (function x -> function y -> x * y) 1 intlist;;

(* a = pairing (c, d), a la Enderton *)
let d (seq:int list) = let newseq = ((List.length seq) - 1)::seq in
	let s = max newseq
	in (factorial s);;

(* TODO: algorithm for chinese remainder theorem to calculate c in terms of d *)

(* TODO: a = pairing (c,d); beta = beta_godel (pairing (c, (d seq))) *)

let remainder_list (c:int) (intlist:int list) = List.map (function x -> c mod x) intlist;;

let beta_godel ((a:int), (i:int)) = let m = (inverse_pairing a) in 
	(proj1 m) mod ((i + 1) * (proj2 m) + 1);;

let beta (seq:int list) (i:int) = 
	let d1 = (d seq) in
	let dlist = (List.map (function i -> 1+ (i+1)*d1) (range 0 ((List.length seq) - 1))) in
	let p = product dlist in 
	let c = List.find (function x -> (remainder_list x dlist) = seq) (range 0 (p-1))
	in c mod (1 + (i+1)*d1);;


let beta2 (seq:int list) (i:int) = 
	let d1 = (d seq) in
	let dlist = (List.map (function i -> 1+ (i+1)*d1) (range 0 ((List.length seq) - 1))) in
	let p = product dlist in (print_int p);
	let c = List.find (function x -> (remainder_list x dlist) = seq) (range 0 (p-1))
	in beta_godel ((pairing (c,d1)), i);;
(*TODO: check 0 <= c < p; remainder_list c dlist; one of these will be the sequence *)
