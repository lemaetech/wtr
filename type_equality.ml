type _ tid = ..

type _ tid += Int : int tid | Float : float tid | Bool : bool tid

type ('a, 'b) teq = Teq : ('a, 'a) teq

let eq : type a b. a tid -> b tid -> (a, b) teq option =
 fun a b ->
  match (a, b) with
  | Int, Int -> Some Teq
  | Float, Float -> Some Teq
  | Bool, Bool -> Some Teq
  | _ -> None

type _ key =
  | K :
      { decode : string -> 'c option
      ; name : string
      ; tid : 'c tid
      }
      -> 'c key

let eq_key : type a b. a key -> b key -> (a, b) teq option =
 fun (K { tid = atid; _ }) (K { tid = btid; _ }) ->
  match (atid, btid) with
  | Int, Int -> Some Teq
  | Float, Float -> Some Teq
  | Bool, Bool -> Some Teq
  | _ -> None
