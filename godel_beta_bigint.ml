(* Godel's Beta-function in OCaml, to see how it really works *)
(* Shane Steinert-Threlkeld, 08/22/2010 *)

#load "nums.cma";;
open Big_int;;

(* This is what the beta function does, but provides no insight. *)
(* One can look at implementation of List.nth to see if it uses Godel's beta to calculate. *)
let beta_ocaml (seq:int list) (i:int) = List.nth seq i;;

(* HELPERS; these are not explicitly mathematical *)

(* This is Cantor's classic function from N*N -> N *)
let pairing ((a:big_int), (b:big_int)) = big_int_of_string (string_of_float (0.5 *. float_of_big_int
        (add_big_int (mult_big_int (add_big_int a b) (add_big_int a b))  (mult_big_int
        (big_int_of_int 3) (add_big_int a b)))));;

(* Inverse of it; after all, Cantor showed that N*N and N are equinumerous, i.e. the pairing function is 1-1 and onto *)
(* NOTE: div_big_int is like doing floor of a float division, i.e. q in a = q*b
 * + r *)
let inverse_pairing (z:big_int) =
	let w = (div_big_int (sub_big_int (sqrt_big_int (
        (add_big_int (mult_big_int (big_int_of_int 8) z) unit_big_int)))
        unit_big_int) (big_int_of_int 2))
	in let t = (div_big_int (add_big_int (mult_big_int w w) w) (big_int_of_int 2))
		in let x = (sub_big_int z t) in
		(x, sub_big_int w x);;

(* While I know that these projection functions will only be used on integers, I'm defining them with polymorphic types for maximum generality. *)
let proj1 (x,y) = x;;
let proj2 (x,y) = y;;

(* Generic factorial algorithm *)
let rec factorial (n:big_int) =
	if eq_big_int n zero_big_int then unit_big_int else
                mult_big_int n (factorial (sub_big_int n unit_big_int));;

(* List a ... b *)
let rec range a b =
	if gt_big_int a b then []
	else a :: range (add_big_int a unit_big_int) b;;

(* Calculates the maximum value of an integer list *)
let max (intlist:big_int list) = List.fold_left (function x -> function y -> if
        gt_big_int x y then x else y) zero_big_int intlist;;

(* product of a list of integers *)
let product (intlist:big_int list) = List.fold_left (function x -> function y ->
        mult_big_int x y) (big_int_of_int 1) intlist;;

(* a = pairing (c, d), a la Enderton *)
let d (seq:big_int list) = let newseq = (big_int_of_int (List.length seq))::seq in
	let s = max newseq
	in (factorial s);;

(* TODO: algorithm for chinese remainder theorem to calculate c in terms of d *)

(* TODO: a = pairing (c,d); beta = beta_godel (pairing (c, (d seq))) *)

let remainder_list (c:big_int) (intlist:big_int list) = List.map (function x ->
        mod_big_int c x) intlist;;

let beta_godel ((a:big_int), (i:big_int)) = let m = (inverse_pairing a) in 
	(mod_big_int (proj1 m) (add_big_int (mult_big_int (add_big_int i
        unit_big_int) (proj2 m)) unit_big_int));;

let beta (seq:big_int list) (i:int) = 
	let d1 = (d seq) in
	let dlist = (List.map (function i -> add_big_int unit_big_int
                (mult_big_int (add_big_int i unit_big_int) d1)) (range
                zero_big_int (sub_big_int
                (big_int_of_int (List.length seq)) unit_big_int))) in
	let p = product dlist in 
	let c = List.find (function x -> (remainder_list x dlist) = seq) (range
                zero_big_int (sub_big_int p unit_big_int))
	in mod_big_int c (List.nth dlist i);;


let beta2 (seq:big_int list) (i:int) = 
	let d1 = (d seq) in
	let dlist = (List.map (function i -> add_big_int unit_big_int
                (mult_big_int (add_big_int i unit_big_int) d1)) (range
                zero_big_int (sub_big_int
                (big_int_of_int (List.length seq)) unit_big_int))) in
	let p = product dlist in
	let c = List.find (function x -> (remainder_list x dlist) = seq) (range 
                zero_big_int (sub_big_int p unit_big_int))
	in beta_godel ((pairing (c, d1)), big_int_of_int i);;
(*TODO: check 0 <= c < p; remainder_list c dlist; one of these will be the sequence *)
