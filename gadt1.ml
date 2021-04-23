type ('ty, 'v) t =
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

let printf : ('ty, 'v) t -> 'a = fun fmt -> kprintf (fun x -> x) fmt

let fmt1 = String (Constant (" | ", String (Constant (" ", Int End))))

let f1 = printf fmt1 "hello" "hello" 12

let fmt2 = Constant ("||", Constant ("   ", End))

let f = printf fmt2

let rec apply : type ty res. ty -> (ty, res) t -> res =
 fun f -> function
  | End -> f
  | Constant (_, fmt) -> apply f fmt
  | String fmt -> apply (f "hello") fmt
  | Int fmt -> apply (f 10) fmt
