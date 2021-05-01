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

let a_int = K { decode = int_of_string_opt; name = "int" }

let b_int = K { decode = int_of_string_opt; name = "int" }

let c_int = K { decode = float_of_string_opt; name = "float" }

let _c = eq a_int c_int

type a = A : 'a key -> a

type b = B : 'a key * 'a -> b
