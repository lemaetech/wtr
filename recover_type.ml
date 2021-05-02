module Eq = struct
  type (_, _) t = Eq : ('a, 'a) t
end

type _ key =
  | K :
      { decode : string -> 'c option
      ; name : string
      }
      -> 'c key

let eq : type a b. a key -> b key -> (a, a) Eq.t option =
 fun t t' ->
  match (t, t') with
  | K { name; _ }, K { name = name'; _ } when String.equal name name' ->
    Some Eq.Eq
  | _, _ -> None

type a = A : 'a key -> a

type b = B : 'a key * 'a -> b

type c = C : 'a key * ('a -> float option) -> c

let decode : a -> string -> b =
 fun (A (K key)) s ->
  let v =
    match key.decode s with
    | Some v -> v
    | None -> failwith "invalid value"
  in
  B (K key, v)

(* --- Equality tests --- *)
let a_int : int key = K { decode = int_of_string_opt; name = "int" }

let b_int : int key = K { decode = int_of_string_opt; name = "int" }

let a_float : float key = K { decode = float_of_string_opt; name = "float" }

let _c : (float, float) Eq.t option = eq a_float a_int
(* None *)

(* --- Try recovering values --- *)
let aa : a = A a_int

let bb : b = decode aa "123"

let to_float : c -> b -> float option =
 fun (C (key, f)) (B (key', v)) ->
  match eq key key' with
  | Some Eq.Eq -> Some (f v)
  (*
     File "recover_type.ml", line 52, characters 12-13:
     52 |     Some (f v)
                      ^
     Error: This expression has type $B_'a but an expression was expected of type
              $C_'a
  *)
  | None -> None
