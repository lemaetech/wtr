module Order = struct
  type (_, _) t =
    | Lt : ('a, 'b) t
    | Eq : ('a, 'a) t
    | Gt : ('a, 'b) t
end

module type KEY = sig
  type _ t

  val compare : 'a t -> 'b t -> ('a, 'b) Order.t
end

module Make (Key : KEY) = struct
  type 'a key = 'a Key.t

  type k = K : 'a key -> k

  type b = B : 'a key * 'a -> b

  module M = Map.Make (struct
    type t = k

    let compare (K a) (K b) =
      match Key.compare a b with
      | Order.Lt -> -1
      | Order.Eq -> 0
      | Order.Gt -> 1
  end)

  type t = b M.t

  let get : type a. a key -> t -> a =
   fun k m ->
    match M.find (K k) m with
    | B (k', v) -> (
      match Key.compare k k' with
      | Order.Eq -> v
      | _ -> assert false)
end
