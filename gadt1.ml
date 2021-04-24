(** Some experiments with GADTs. Based on article at
    https://drup.github.io/2016/08/02/difflists/ *)
type ('ty, 'v) t = ..

type ('ty, 'v) t +=
  | End : ('v, 'v) t
  | Constant : string * ('ty, 'v) t -> ('ty, 'v) t
  | String : ('ty, 'v) t -> (string -> 'ty, 'v) t
  | Int : ('ty, 'v) t -> (int -> 'ty, 'v) t

let rec kprintf : type ty res. (string -> res) -> (ty, res) t -> ty =
 fun k -> function
  | End -> k ""
  | Constant (const, fmt) -> kprintf (fun str -> k @@ const ^ str) fmt
  | String fmt ->
    let f s = kprintf (fun str -> k @@ s ^ str) fmt in
    f
  | Int fmt ->
    let f i = kprintf (fun str -> k @@ string_of_int i ^ str) fmt in
    f
  | _ -> assert false

let sprintf : ('ty, 'v) t -> 'a = fun fmt -> kprintf (fun x -> x) fmt

let fmt1 = String (Constant (" | ", String (Constant (" ", Int End))))

let f1 = sprintf fmt1 "hello" "hello" 12
(* val f1 : string = "hello | hello 12" *)

let fmt2 = String (Constant ("   ", End))

let f = sprintf fmt2
(* val f : string -> string = <fun> *)

let rec apply : type ty res. (ty, res) t -> ty -> res =
 fun fmt f ->
  match fmt with
  | End -> f
  | Constant (_, fmt) -> apply fmt f
  | String fmt -> apply fmt (f "hello")
  | Int fmt -> apply fmt (f 10)
  | _ -> assert false

let a = apply fmt1 (fun s1 s2 i -> s1 ^ " || " ^ s2 ^ " || " ^ string_of_int i)
(* - : string = "hello || hello || 10" *)

type param =
  | Int of int
  (* | Float of float *)
  | String of string

let rec apply_params : type ty res. (ty, res) t -> ty -> param list -> res =
 fun fmt f params ->
  match (fmt, params) with
  | End, [] -> f
  | Constant (_, fmt), params -> apply_params fmt f params
  | String fmt, String s :: params -> apply_params fmt (f s) params
  | Int fmt, Int i :: params -> apply_params fmt (f i) params
  | _, _ -> failwith "Not matched"

let _a =
  apply_params fmt1
    (fun s1 s2 i -> s1 ^ " || " ^ s2 ^ " || " ^ string_of_int i)
    [ String "a"; String "b"; Int 100 ]
(* val a : string = "a || b || 100" *)

(* type ('ty, 'v) t1 = .. *)
