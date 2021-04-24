type void = |

module Make (T : sig
  type 'a t
end) =
struct
  type 'a t =
    | [] : void t
    | ( :: ) : 'a T.t * 'b t -> ('a -> 'b) t
end

(** Hl - Heterogenous list, let c = [ "s"; 1; 2.3; true ] *)
module Hlist = Make (struct
  type 'a t = 'a
end)

let hl = Hlist.[ 1; 1.2; "hello" ]
(* val hl : (int -> float -> string -> void) Hlist.t = Hlist.(::) (1, Hlist.(::)
   (1.2, Hlist.(::) ("hello", Hlist.[]))) *)

type 'a witness = ..

type 'a witness +=
  | Int : int witness
  | Float : float witness
  | Bool : bool witness

module Witness = Make (struct
  type 'a t = 'a witness
end)

let a = Witness.[]

(** [pack] packs ['a t] and ['a witness] into a monomorphic type so that we can
    create ['a t] at runtime dynamically. *)
type pack = Pack : 'a Hlist.t * 'a Witness.t -> pack

let rec parse : int list -> pack = function
  | [] -> Pack (Hlist.[], Witness.[])
  | x :: l ->
    let (Pack (v, t)) = parse l in
    if x = 0 then
      Pack (Hlist.(10. :: v), Witness.(Float :: t))
    else if x = 1 then
      Pack (Hlist.(true :: v), Witness.(Bool :: t))
    else
      Pack (Hlist.(10 :: v), Witness.(Int :: t))

module Make2 (T : sig
  type 'a t
end) =
struct
  type 'a t =
    | [] : void t
    | ( :: ) : 'a T.t * 'b t -> ('a -> 'b) t
end

module HL = Make2 (struct
  type 'a t = 'a
end)

type 'a ty =
  | Int : int ty
  | Float : float ty

module HLT = Make2 (struct
  type 'a t = 'a ty
end)

type dyn = Typed : 'a HL.t * 'a HLT.t -> dyn

let rec gen : int -> dyn =
 fun n ->
  if n = 0 then
    Typed (HL.[], HLT.[])
  else
    let (Typed (v, t)) = gen (n - 1) in
    if Random.float 1. < 0.5 then
      Typed (HL.(Random.float 1. :: v), HLT.(Float :: t))
    else
      Typed (HL.(Random.int 10 :: v), HLT.(Int :: t))

let rec add : type a. a HLT.t -> a HL.t -> float =
 fun ts xs ->
  match (ts, xs) with
  | [], [] -> 0.
  | Int :: ts, n :: xs -> float n +. add ts xs
  | Float :: ts, x :: xs -> x +. add ts xs

let test =
  let (Typed (xs, ts)) = gen 100 in
  add ts xs
