(* Q1 (a) *)

exception Failure;

fun sum(m, n) =
if (m > n) then raise Failure
else
if n = 0 then m
else (m + n) + sum(m, n-1);

(* Q1 (b) *)
fun fact(0) = 1
  | fact(n) = n*fact(n-1);

fun bin_coeff1 ( n, r) =
if (n < r) then raise Failure
else
fact(n) div (fact(n-r) * fact(r));

fun bin_coeff (n, r) =
if (n < r) then raise Failure
else
if (r = 1) then n
else (n*bin_coeff1(n-1, r-1)) div r;

(* Q2 (a) *)

datatype meridien = AM | PM;
fun time_in_mins(hour : int, min : int, f : meridien) =
if f = AM then
hour*60 + min
else
hour*60 + min + 720;

fun op compare (t1 : int * int * meridien, t2 : int * int * meridien) = time_in_mins t1 <= time_in_mins t2;

(* Q2 (b) *)

fun time_in_mins_record ({ hour : int, min : int, f : meridien }) =
if f = AM then
hour*60 + min
else
hour*60 + min + 720;

fun op compare_record (t1 : { hour : int, min : int, f : meridien }, t2 : { hour : int, min : int, f : meridien } ) = time_in_mins_record t1 <= time_in_mins_record t2;

(* Q3 *)

(* !!NOTE!!- Assuming polynomial represented as array with entries in decreasing order of degree
For example - x^2 - 2x + 1 would be represented as [(1,2), (~2, 1), (1, 0)]*)

(* Adding Polynomials *)
fun add(poly1 : (real * int) list, [] ) : (real * int) list = poly1
  | add([], poly2 : (real * int) list ) : (real * int) list = poly2
  | add( ((h1c, h1d) :: t1) : (real * int) list, ((h2c, h2d) :: t2) : (real * int) list ) : (real * int) list =
  if (h1d > h2d) then (h1c, h1d) :: add(t1, ((h2c, h2d) :: t2) )
  else if (h1d < h2d) then (h2c, h2d) :: add(((h1c, h1d) :: t1), t2)
  else ( h1c + h2c, h1d ) :: add(t1, t2);

(* Multiplying Polynomials *)
fun multiply_single((coeff, deg) : (real * int), [] ) : (real * int) list = []
  | multiply_single((coeff, deg) : (real * int), ((h2c, h2d) :: t2) : (real * int) list ) : (real * int) list =
  (coeff*h2c, h2d + deg) :: multiply_single((coeff, deg), t2);

fun multiply(poly1 : (real * int) list, [] ) : (real * int) list = []
  | multiply([], poly2 : (real * int) list ) : (real * int) list = []
  | multiply( ((h1c, h1d) :: t1) : (real * int) list, t2 : (real * int) list ) : (real * int) list =
  add(multiply_single((h1c, h1d), t2), multiply(t1, t2));

(* fun remove_zero([]) : (real * int) list = []
  | remove_zero( ((coeff, deg) :: tail) : (int * int) list) : (int * int) list =
  if coeff <> 0.0 then (coeff, deg) :: remove_zero(tail)
  else remove_zero(tail); *)

(* fun multiply(poly1 : (real * int) list, poly2 : (real * int) list ) : (real * int) list =
remove_zero( multiply_main(poly1, poly2) ); *)


(* Q4 *)
infix ++;
infix **;
infix //;

structure Complex = struct
fun (a, b) ++ (c, d) : (real * real) = (a+c, b+d);
fun (a, b) ** (c, d) : (real * real) = (a*c - b*d, a*d + b*c);
fun inv (a, b) : (real * real) = (a / (a*a + b*b), ~b / (a*a + b*b));
fun (a, b) // (c, d) : (real * real) = (a, b) ** inv(c, d);
end;

open Complex;
