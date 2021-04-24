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

module Witness = struct
  type 'a witness = ..

  type 'a witness +=
    | Int : int witness
    | Float : float witness
    | Bool : bool witness

  include Make (struct
    type 'a t = 'a witness
  end)
end

let a = Witness.[]
(* val a : void Witness.t = Witness.[] *)

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
