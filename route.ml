type ('ty, 'v) route =
  | End : ('v, 'v) route
  | Constant : string * ('ty, 'v) route -> ('ty, 'v) route
  | String : ('ty, 'v) route -> (string -> 'ty, 'v) route
  | Int : ('ty, 'v) route -> (int -> 'ty, 'v) route
  | Float : ('ty, 'v) route -> (int -> 'ty, 'v) route
  | Bool : ('ty, 'v) route -> (bool -> 'ty, 'v) route

let route1 = String (Constant (" | ", String (Constant (" ", Int End))))

type pack = Pack : ('ty, 'v) route -> pack

let rec parse : int list -> pack = function
  | [] -> Pack End
  | x :: l ->
    let (Pack fmt) = parse l in
    if x = 0 then
      Pack (Int fmt)
    else if x = 1 then
      Pack (Float fmt)
    else if x = 2 then
      Pack (Bool fmt)
    else
      Pack (Constant ("hello", fmt))

let rec pp : pack -> unit = function
  | Pack End -> ()
  | Pack (Constant (c, fmt)) ->
    print_string ("/" ^ c);
    pp (Pack fmt)
  | Pack (String fmt) ->
    print_string "/:string";
    pp (Pack fmt)
  | Pack (Int fmt) ->
    print_string "/:int";
    pp (Pack fmt)
  | Pack (Float fmt) ->
    print_string "/:float";
    pp (Pack fmt)
  | Pack (Bool fmt) ->
    print_string "/:bool";
    pp (Pack fmt)
