type ('ty, 'v) t =
  | End : ('v, 'v) t
  | Constant : string * ('ty, 'v) t -> ('ty, 'v) t
  | Hole : ('ty, 'v) t -> (string -> 'ty, 'v) t

let rec kprintf : type ty res. (string -> res) -> (ty, res) t -> ty =
 fun k -> function
  | End -> k ""
  | Constant (const, fmt) -> kprintf (fun str -> k @@ const ^ str) fmt
  | Hole fmt ->
    let f s = kprintf (fun str -> k @@ s ^ str) fmt in
    f

let printf fmt = kprintf (fun x -> x) fmt

let fmt = Hole (Constant (" | ", Hole End))

let a = printf fmt
