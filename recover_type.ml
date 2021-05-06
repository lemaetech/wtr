module Eq = struct
  type (_, _) t = Eq : ('a, 'a) t
end

type _ tid = ..

type _ tid += Int : int tid | Float : float tid | Bool : bool tid

type _ key =
  | K :
      { decode : string -> 'c option
      ; name : string
      ; tid : 'c tid
      }
      -> 'c key

let eq : type a b. a key -> b key -> (a, b) Eq.t option =
 fun (K { tid = atid; _ }) (K { tid = btid; _ }) ->
  match (atid, btid) with
  | Int, Int -> Some Eq.Eq
  | Float, Float -> Some Eq.Eq
  | Bool, Bool -> Some Eq.Eq
  | _ -> None

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
let a_int : int key = K { decode = int_of_string_opt; tid = Int; name = "int" }

let b_int : int key = K { decode = int_of_string_opt; tid = Int; name = "int" }

let a_float : float key =
  K { decode = float_of_string_opt; tid = Float; name = "float" }

let _c = eq a_float a_int
(* None *)

(* --- Try recovering values --- *)
let aa : a = A a_int

let bb : b = decode aa "123"

let to_float : c -> b -> float option =
 fun (C (key, f)) (B (key', v)) ->
  match eq key key' with
  | Some Eq.Eq -> f v
  | None -> None

let cc_int : c =
  C
    ( a_int
    , fun i ->
        try Some (float_of_int i) with
        | _ -> None )

let f = to_float cc_int bb
(* - : float option = Some 123. *)
