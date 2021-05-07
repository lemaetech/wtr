module Arg = struct
  type 'a witness = ..
  type (_, _) eq = Eq : ('a, 'a) eq

  module type Ty = sig
    type t

    val witness : t witness
    val eq : 'a witness -> ('a, t) eq option
  end

  type 'a id = (module Ty with type t = 'a)

  let new_id (type a) () =
    let module Ty = struct
      type t = a
      type 'a witness += Ty : t witness

      let witness = Ty

      let eq (type b) : b witness -> (b, t) eq option = function
        | Ty -> Some Eq
        | _ -> None
    end in
    (module Ty : Ty with type t = a)

  let eq : type a b. a id -> b id -> (a, b) eq option =
   fun (module TyA) (module TyB) -> TyB.eq TyA.witness

  type 'a t =
    { name : string (* name e.g. int, float, bool, string etc *)
    ; decode : string -> 'a option
    ; id : 'a id
    }

  let create ~name ~decode =
    let id = new_id () in
    { name; decode; id }

  let int = create ~name:"int" ~decode:int_of_string_opt
  let float = create ~name:"float" ~decode:float_of_string_opt
  let bool = create ~name:"bool" ~decode:bool_of_string_opt
  let string = create ~name:"string" ~decode:(fun a -> Some a)
end

(** [('a, 'b) path] represents a uniform resource identifier. The variant
    members describe the path component types. *)
type ('a, 'b) path =
  | End : ('b, 'b) path
  | Literal : string * ('a, 'b) path -> ('a, 'b) path
      (** Uri path literal string component eg. 'home' in '/home' *)
  | Arg : 'c Arg.t * ('a, 'b) path -> ('c -> 'a, 'b) path
      (** Uri path argument component, i.e. the value is determined during
          runtime, eg. ':int' in '/home/:int' *)

let[@warning "-32"] end_ : ('b, 'b) path = End

let[@warning "-32"] lit : string -> ('a, 'b) path -> ('a, 'b) path =
 fun s path -> Literal (s, path)

let[@warning "-32"] arg a p = Arg (a, p)

type 'c route = Route : ('a, 'c) path * 'a -> 'c route

let ( >- ) : ('a, 'b) path -> 'a -> 'b route = fun path f -> Route (path, f)

(** Defines existential to encode path component type. *)
module Path_type = struct
  type t =
    | PLiteral : string -> t
    | PVar : 'c Arg.t -> t

  let equal a b =
    match (a, b) with
    | PLiteral lit', PLiteral lit -> String.equal lit lit'
    | PVar arg', PVar arg -> (
      match Arg.eq arg.id arg'.id with
      | Some Arg.Eq -> true
      | None -> false )
    | _ -> false

  (* [of_path path] converts [path] to [kind list]. This is done to get around OCaml
     type inference issue when using [path] type in the [add] function below. *)
  let rec of_path : type a b. (a, b) path -> t list = function
    | End -> []
    | Literal (lit, path) -> PLiteral lit :: of_path path
    | Arg (arg, path) -> PVar arg :: of_path path
end

(** ['a t] is a node in a trie based router. *)
type 'a node =
  { route : 'a route option
  ; path : (Path_type.t * 'a node) list
  }

let update_path t path = { t with path }
let empty : 'a node = { route = None; path = [] }

let add t (Route (path, _) as route) =
  let rec loop t = function
    | [] -> { t with route = Some route }
    | path_kind :: path_kinds ->
      List.find_opt
        (fun (path_kind', _) -> Path_type.equal path_kind path_kind')
        t.path
      |> (function
           | Some _ ->
             List.map
               (fun (path_kind', t') ->
                 if Path_type.equal path_kind path_kind' then
                   (path_kind', loop t' path_kinds)
                 else
                   (path_kind', t') )
               t.path
           | None -> (path_kind, loop empty path_kinds) :: t.path )
      |> update_path t in
  loop t (Path_type.of_path path)

type 'a t =
  { route : 'a route option
  ; path : (Path_type.t * 'a t) array
  }

let rec create routes = List.fold_left add empty routes |> compile

and compile : 'a node -> 'a t =
 fun t ->
  { route = t.route
  ; path =
      List.rev t.path
      |> List.map (fun (path_kind, t) -> (path_kind, compile t))
      |> Array.of_list
  }

type decoded_value = D : 'c Arg.t * 'c -> decoded_value

let rec match' t path =
  let rec loop t decoded_values = function
    | [] ->
      Option.map
        (fun (Route (path, f)) ->
          exec_route_handler f (path, List.rev decoded_values) )
        t.route
    | path_token :: path_tokens ->
      let continue = ref true in
      let index = ref 0 in
      let matched_node = ref None in
      while !continue && !index < Array.length t.path do
        match t.path.(!index) with
        | PVar arg, t' -> (
          match arg.decode path_token with
          | Some v ->
            matched_node := Some (t', D (arg, v) :: decoded_values);
            continue := false
          | None -> incr index )
        | PLiteral lit, t' when String.equal lit path_token ->
          matched_node := Some (t', decoded_values);
          continue := false
        | _ -> incr index
      done;
      Option.bind !matched_node (fun (t', decoded_values) ->
          (loop [@tailcall]) t' decoded_values path_tokens ) in
  String.split_on_char '/' path
  |> List.filter (fun tok -> not (String.equal "" tok))
  |> loop t []

and exec_route_handler : type a b. a -> (a, b) path * decoded_value list -> b =
 fun f -> function
  | End, [] -> f
  | Literal (_, path), decoded_values ->
    exec_route_handler f (path, decoded_values)
  | Arg ({ id; _ }, path), D ({ id = id'; _ }, v) :: decoded_values -> (
    match Arg.eq id id' with
    | Some Arg.Eq -> exec_route_handler (f v) (path, decoded_values)
    | None -> assert false )
  | _, _ -> assert false

(* let router = *)
(*   create *)
(*     [ lit "home" (lit "about" end_) >- "about" *)
(*     ; (lit "home" (int end_) >- fun i -> "int " ^ string_of_int i) *)
(*     ; (string (int end_) >- fun s i -> s ^ string_of_int i) *)
(*     ; (lit "home" (float end_) >- fun f -> "float " ^ string_of_float f) *)
(*     ] *)

(* let _m = match' router "/home/100001.1" *)

(* let _m1 = match' router "/home/100001" *)

(* let _m2 = match' router "/home/about" *)
