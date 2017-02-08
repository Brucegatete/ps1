(* 
			 CS 51 Problem Set 1
		     Core Functional Programming
			     Spring 2017
*)

(*======================================================================
Problem 1: Dealing with types

........................................................................
For each of the expressions below, enter a string describing the type
of the expression in the expressions below, replacing the ??? with the
appropriate type. The first one is done for you. Be sure to remove the
comments from each subproblem and to type check it before submission.
......................................................................*)

(*
let prob0 : int = 42 ;;

let prob1a : string = let greet y = "Hello " ^ y in greet "World!";;

let prob1b : 'a option list = [Some 4; Some 2; None; Some 3];;

let prob1c : (('a * 'b) * bool)  = ((None, Some 42.0), true);;
*)

let prob0 = "int";;

let prob1a = "string";;

let prob1b = "'a option list";;

let prob1c = "(('a option * int option) * bool )tuple";;

(*......................................................................
There are several values defined below that do not type check. 

Explain in a comment above each corresponding value why the following
definitions will not type check, and then provide a fixed version of 
each function as an OCaml value (outside of a comment). Your fix should
change the code minimally. 
......................................................................*)

(*
let prob1d : string * int list = [("CS", 51); ("CS", 50)];;
*)
(*This was wrong because of the misplaced paranthesis that caused the function to expect a tuple of a string and an int list.
*)
  let prob1d : (string * int) list = [("CS", 51); ("CS", 50)];;
(*
  
let prob1e : int =
	let add (x, y) = x + y in
	if add (4, 3.9) = 10 then 4 else 2;;
 The expression has a type "float" but the expected expression by Ocaml is an integer
*)
let prob1e : float =
  let add (x, y) = x +. y in
  if add (4., 3.9) = 10. then 4. else 2.;;

(*
  The declared values are meant to be tuples containing two strings (string * string) but most of the values of this function are (string * int). Plus, instead of None, the programmer should have used the integer 0.
let prob1f : (string * string) list =
	[("January", None); ("February", 1); ("March", None); ("April", None);
  ("May", None); ("June", 1); ("July", None); ("August", None);
  ("September", 3); ("October", 1); ("November", 2); ("December", 3)];;
*)

(*The declared values are meant to be tuples containing two strings (string * string) but most of the values of this function are (string * int). Plus, instead of None, the programmer should have used the integer 0.*)
let prob1f : (string * int) list =
  [("January", 0); ("February", 1); ("March", 0); ("April", 0);
  ("May", 0); ("June", 1); ("July", 0); ("August", 0);
  ("September", 3); ("October", 1); ("November", 2); ("December", 3)];;
(*======================================================================
Problem 2 - Writing functions

........................................................................
For each subproblem, you must implement a given function, providing
appropriate unit tests in the accompanying file pset1_tests.ml. You
are provided a high level description as well as a type signature of
the function you must implement. Keep in mind the CS51 style guide and
what you've learned so far about efficiency and elegance. You are
*not* allowed to use library functions (i.e., the List module) for
*this* problem unless you implement the functionality yourself.
......................................................................*)

(*......................................................................
Problem 2a: The function "reversed" takes a list of integers and
returns true if the list is in decreasing order. The empty list is
considered to be reversed in this sense. Consecutive elements of the
same value can be considered equal in a reversed list.

Here is its signature: 
reversed : int list -> bool

Replace the line below with your own definition of "reversed".
......................................................................*)



 let rec reversed_helper x = match x with
    | [] -> true
    | _ :: [] -> true
    | h :: b :: t ->
        if h < b then false
        else reversed_helper(b :: t)

 let reversed x = match x with 
    [] -> true
    | _::[] -> true
    | _::_ -> reversed_helper (x);;


(*......................................................................
Problem 2b: The function "merge" takes two integer lists, each
*sorted* in increasing order, and returns a single merged list in
sorted order.  For example:

merge [1;3;5] [2;4;6];;
- : int list = [1; 2; 3; 4; 5; 6]
merge [1;2;5] [2;4;6];;
- : int list = [1; 2; 2; 4; 5; 6]
merge [1;3;5] [2;4;6;12];;
- : int list = [1; 2; 3; 4; 5; 6; 12]
merge [1;3;5;700;702] [2;4;6;12];;
- : int list = [1; 2; 3; 4; 5; 6; 12; 700; 702]

Here is its signature:
merge : int list -> int list -> int list

Replace the line below with your own definition of "merge".
......................................................................*)

let rec merge x y =
  match x, y with
  [], [] -> []
  | _ :: _, [] -> []
  | [], _ :: _ -> []
  | hx :: tx, hy :: ty ->
  if hx <= hy then hx :: hy :: merge tx ty
else hy :: hx :: merge tx ty ;;
(*......................................................................
Problem 2c: The function "unzip", given a list of integer pairs,
returns a pair of lists, the first of which contains each first
element of each pair, and the second of which contains each second
element.  The returned list should have elements in the order in which
they were provided. For example:

unzip [(6,2);(2,4);(5,6)];;
- : int list * int list = ([6;2;5],[2;4;6])

Here is its signature:
unzip : (int * int) list -> (int list * int list)

Replace the line below with your own definition of "unzip".
......................................................................*)

let rec rev l = 
  match l with
  [] -> []
  | h :: t -> rev t @ [h]
let unzip l = 
  let rec unzip_helper (l1,l2) = function
    | [] -> rev l1, rev l2
    | (x,y) :: tl -> unzip_helper (x::l1, y::l2) tl
  in 
  unzip_helper ([],[]) l;;
(*......................................................................
Problem 2d: The function "variance" takes a float list and returns
None if the list has fewer than two elements. Otherwise, it should
return Some of the variance of the floats. Recall that the variance of
a sequence of numbers is given by the following equation:
						
	1/(n-1) * sum (x_i - m)^2

where n indicates the number of elements in the list, m is the
arithmetic mean of the list, and x_i is element in the ith index of
the list. If you want to compare your output with an online
calculator, make sure you find one that calculates the (unbiased)
sample variance.  For example:

variance [1.0; 2.0; 3.0; 4.0; 5.0];;
- : float option = Some 2.5
variance [1.0];;
- : float option = None

Remember to use the floating point version of the arithmetic operators
when operating on floats (+., *., etc). The function "float" can
convert ("cast") an int to a float.

Here is the signature of "variance":
float list -> float option 							

Replace the line below with your own definition of "variance".
......................................................................*)
let variance = (fun _ -> failwith "variance not implemented") ;;

  (*let rec variance l = 
  let rec sum l =
  match l with 
  [] -> 0.
  | h :: t -> h +. sum t in 
  let rec lenght l =
  match l with
  [] -> 0.
  | _h :: t -> 1. +. lenght t in 

  match l with 
  [] -> None
  |[_] -> None
  | h :: t -> Some((1. /. (lenght l -. 1.)) *. (h -. (sum l /. lenght l)) *. (h -. (sum l /. lenght l)) +. variance t );;*)



(*......................................................................
Problem 2e: The function "few divisors" takes two integers, x and y, and
returns true if x has fewer than y divisors (including 1 and x). Note:
this is NOT the same as x having fewer divisors than y. For example:

few_divisors 17 3;;
- : bool = true
few_divisors 4 3;;
- : bool = false
few_divisors 4 4;;
- : bool = true

Do not worry about negative integers at all. We will not test your code
using negative values for x and y, and do not consider negative integers
for divisors (i.e. -2 being a divisor for 4). 

Here is its signature:
few_divisors : int -> int -> bool 

Replace the line below with your own definition of "few_divisors".
......................................................................*)


  let rec mod_0 n l=
  match l with
  [] -> []
  | h :: t -> n mod h :: mod_0 n t

let rec nber_div l =
  match l with 
  [] -> 0
  | _h :: t -> 
  if _h = 0 then 1 + nber_div t
else nber_div t

let rec lower n =
  match n with
  0 -> []
  | a -> a :: lower (n -1)

let divisors n =
  match n with 
  0 -> 1
  | 1 -> 0 
  | n -> nber_div(mod_0 n (lower n))

let few_divisors n l =
if divisors n < l then true else false;;


(*......................................................................
Problem 2f: The function "concat_list" takes two arguments: sep, a
string, and lst, a string list. It returns one string with all the
elements of lst concatenated together but separated by the string
sep. For example:

concat_list ", " ["Greg"; "Anna"; "David"];;
- : string = "Greg, Anna, David"
concat_list "..." ["Moo"; "Baaa"; "Quack"];;
- : string = "Moo...Baaa...Quack"
concat_list ", " [];;
- : string = ""
concat_list ", " ["Moo"];;
- : string = "Moo"

Here is its signature:
concat_list : string -> string list -> string

Replace the line below with your own definition of "concat_list"
......................................................................*)

let rec concat_list (n: string) (m: string list) : (string)=
  match n, m with
  _n, [] -> ""
  | _n, _b :: [] -> _b
  | _n, _b :: t -> _b ^ _n  ^  concat_list n t;;



(*......................................................................
Problem 2g: One way to compress a list of characters is to use
run-length encoding. The basic idea is that whenever we have repeated
characters in a list such as

  ['a'; 'a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'c'; 'd'; 'd'; 'd'; 'd'] 

we can (sometimes) represent the same information more compactly as a
list of pairs like 

  [(5, 'a'); (3, 'b'); (1, 'c'); (4, 'd')]      . 

Here, the numbers represent how many times the character is
repeated. For example, the first character in the string is 'a' and it
is repeated 5 times, followed by 3 occurrences of the character 'b',
followed by one 'c', and finally 4 copies of 'd'.

Write a function "to_run_length" that converts a list of characters
into the run-length encoding, and then write a function
"from_run_length" that converts back. Writing both functions will make
it easier to test that you've gottem them right.

Here are their prototypes/signatures:

  to_run_length : char list -> (int * char) list
  from_run_length : (int * char) list -> char list

Replace the lines below with your own definition of "to_run_length"
and "from_run_length".
......................................................................*)


  
  let rec filter (l: char list) (a : char) =
  match l with 
  [] -> []
  | h :: t -> if h = a then filter t a  
  else l
  let rec counter (a: char) (l: char list) : int =
  match l with
  [] -> 0
  | h:: t -> if h = a then 1 + counter a t
  else 0
  let rec to_run_length (l: char list) : ((int * char) list) =
  match l with
  [] -> []
  | a :: t -> ((counter a l), a) :: to_run_length (filter t a);;




let rec long (x, y) =
    match (x, y) with
    0, _ -> []  
  | x, y -> y :: long (x -1, y) 

let rec from_run_length l = 
  match l with
  [] -> []
  | h:: t -> long h @ from_run_length t;;

(*======================================================================
Problem 3: Challenge problem: Permutations

........................................................................
The function "permutations" takes a list of integers and should
return a list containing every permutation of the list. For example:

  permutations [1; 2; 3] =
  - : int list list = [[1; 2; 3]; [2; 1; 3]; [2; 3; 1]; [1; 3; 2]; 
  [3; 1; 2]; [3; 2; 1]]

It doesn't matter what order the permutations appear in the returned
list.  Note that if the input list is of length n, then the answer
should be of length n! (that is, the factorial of n).

Hint: One way to do this is to write an auxiliary function, interleave
: int -> int list -> int list list, that yields all interleavings of
its first argument into its second. For example:

  interleave 1 [2; 3] = 
  - : int list list = [ [1; 2; 3]; [2; 1; 3]; [2; 3; 1] ]

You may also use list module functions for this question and may find 
List.map and List.concat helpful. 

Here is the signature of permutations:

  permutations : int list -> int list list

Replace the line below with your own definition of "permutations".
......................................................................*)

let permutations = (fun _ -> failwith "permutations not implemented") ;;


(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete.  We care
about your responses and will use them to help guide us in creating
future assignments.
......................................................................*)

let minutes_spent_on_pset () : int = 900 ;;
