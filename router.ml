open! Core

(** [('a, 'b) uri] represents a uniform resource identifier. The variant members
    describe the uri path component types. *)
type ('a, 'b) uri =
  | End : ('b, 'b) uri
  | Literal : string * ('a, 'b) uri -> ('a, 'b) uri
      (** Uri literal string component eg. 'home' in '/home' *)
  | Var : 'c var * ('a, 'b) uri -> ('c -> 'a, 'b) uri
      (** Uri variable component, i.e. the value is determined during runtime,
          eg. ':int' in '/home/:int' *)

and 'c var =
  { decode : string -> 'c option
  ; encode : 'c -> string
  ; name : string (* name e.g. :int, :float, :bool, :string etc *)
  }

let end_ : ('b, 'b) uri = End

let lit : string -> ('a, 'b) uri -> ('a, 'b) uri = fun s uri -> Literal (s, uri)

let var decode encode name uri = Var ({ encode; decode; name }, uri)

let string : ('a, 'b) uri -> (string -> 'a, 'b) uri =
 fun uri -> var (fun s -> Some s) Fun.id ":string" uri

let int : ('a, 'b) uri -> (int -> 'a, 'b) uri =
 fun uri -> var int_of_string_opt string_of_int ":int" uri

let float : ('a, 'b) uri -> (float -> 'a, 'b) uri =
 fun uri -> var float_of_string_opt string_of_float ":float" uri

let bool : ('a, 'b) uri -> (bool -> 'a, 'b) uri =
 fun uri -> var bool_of_string_opt string_of_bool ":bool" uri

(** [uri_kind] encodes uri kind/type. *)
type uri_kind =
  | KLiteral : string -> uri_kind
  | KVar : kvar -> uri_kind

and kvar = V : 'c var -> kvar

(* [kind uri] converts [uri] to [kind list]. This is done to get around OCaml
   type inference issue when using [uri] type in the [add] function below. *)
let rec uri_kind : type a b. (a, b) uri -> uri_kind list = function
  | End -> []
  | Literal (lit, uri) -> KLiteral lit :: uri_kind uri
  | Var (var, uri) -> KVar (V var) :: uri_kind uri

(** ['c route] is a uri and its handler. ['c] represents the value returned by
    the handler. *)
type 'c route = Route : ('a, 'c) uri * 'a -> 'c route

(** [p @-> route_handler] creates a route from uri [p] and [route_handler]. *)
let ( @-> ) : ('a, 'b) uri -> 'a -> 'b route = fun uri f -> Route (uri, f)

(** ['a t] is a trie based router where ['a] is the route value. *)
type 'a t =
  | Node :
      { route : 'a route option
      ; literals : 'a t String.Map.t
      ; vars : (kvar * 'a t) Queue.t
      }
      -> 'a t

let empty_with route =
  Node { route; literals = String.Map.empty; vars = Queue.create () }

let empty = empty_with None

let add : 'b route -> 'b t -> 'b t =
 fun route t ->
  let (Route (uri, _)) = route in
  let rec loop : 'b t -> uri_kind list -> 'b t =
   fun (Node t) uri_kinds ->
    match uri_kinds with
    | [] -> Node { t with route = Some route }
    | KLiteral lit :: uri_kinds ->
      let literals =
        String.Map.change t.literals lit ~f:(function
          | Some t' -> Some (loop t' uri_kinds)
          | None -> Some (loop empty uri_kinds))
      in
      Node { t with literals }
    | KVar kvar :: uri_kinds ->
      let (V var) = kvar in
      (Queue.find t.vars ~f:(fun (V var', _) -> String.equal var.name var'.name)
      |> function
      | Some (_, t') -> Queue.enqueue t.vars (kvar, loop t' uri_kinds)
      | None -> Queue.enqueue t.vars (kvar, loop empty uri_kinds));
      Node t
  in
  loop t (uri_kind uri)

(* Represents decoded value [c] in [Some c]. [Some c] is returned by [decode]
   function of ['c var]. We use Obj.t to get around the issue of 'value escaping
   its scope' in [kvar] GADT when it is used in [match'] function. *)
type decoded_value = Obj.t

let rec match' : 'b t -> string -> 'b option =
 fun t uri ->
  let uri_tokens =
    String.rstrip uri ~drop:(function
      | '/' -> true
      | _ -> false)
    |> String.split ~on:'/'
  in
  let rec loop (Node t) decoded_values uri_tokens =
    match uri_tokens with
    | [] ->
      Option.map t.route ~f:(fun (Route (uri, f)) -> apply uri f decoded_values)
    | uri_token :: uri_tokens -> (
      (* Check if one of the vars are matched first. If none is matched then
         match literals. The route that is added first is evaluated first. *)
      let var_matched =
        Queue.fold_until t.vars ~init:None
          ~f:(fun _acc (V var, t') ->
            match var.decode uri_token with
            | Some v -> Stop (Some (Obj.repr v, t'))
            | None -> Continue None)
          ~finish:(fun _ -> None)
      in
      match var_matched with
      | Some (value, t') ->
        (loop [@tailcall]) t' (value :: decoded_values) uri_tokens
      | None ->
        Option.bind (String.Map.find t.literals uri_token) ~f:(fun t' ->
            (loop [@tailcall]) t' decoded_values uri_tokens))
  in
  loop t [] uri_tokens

and apply : type a b. (a, b) uri -> a -> decoded_value list -> b =
 fun uri f vars ->
  match (uri, vars) with
  | End, [] -> f
  | Literal (_, uri), vars -> apply uri f vars
  | Var (var, uri), p :: vars -> apply uri (f @@ to_var_type var p) vars
  | _, _ -> assert false

and to_var_type : type c. c var -> decoded_value -> c = fun _ v : c -> Obj.obj v

let r1 = string (int end_) @-> fun (s : string) (i : int) -> s ^ string_of_int i

let r2 : string route = lit "home" (lit "about" end_) @-> ""

let r3 : string route =
  lit "home" (int end_) @-> fun (i : int) -> string_of_int i

let r4 : string route =
  lit "home" (float end_) @-> fun (f : float) -> string_of_float f

(** This should give error (we added an extra () var in handler) but it doesn't.
    It only errors when adding to the router.*)
let r5 =
  string (int end_) @-> fun (s : string) (i : int) () -> s ^ string_of_int i

let router = empty |> add r1 |> add r2 |> add r3 |> add r4

(* |> add r5  *)
(* This errors *)
