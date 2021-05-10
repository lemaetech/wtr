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

  let string = create ~name:"string" ~decode:(fun a -> Some a)

  let bool = create ~name:"bool" ~decode:bool_of_string_opt
end

type 'a arg = 'a Arg.t

let create_arg = Arg.create

module type Arg = sig
  type t

  val t : t arg
end

(** [('a, 'b) path] represents a uniform resource identifier. The variant
    members describe the path component types.

    - Literal Uri path literal string component eg. 'home' in '/home'
    - Arg Uri path argument component, i.e. the value is determined during
      runtime, eg. ':int' in '/home/:int' *)
type ('a, 'b) path =
  | Nil : ('b, 'b) path
  | Full_splat : ('b, 'b) path
  | Trailing_slash : ('b, 'b) path
  | Literal : string * ('a, 'b) path -> ('a, 'b) path
  | Arg : 'c Arg.t * ('a, 'b) path -> ('c -> 'a, 'b) path

type 'c route = Route : ('a, 'c) path * 'a -> 'c route

let ( >- ) : ('a, 'b) path -> 'a -> 'b route = fun path f -> Route (path, f)

module Path_type = struct
  (** Defines existential to encode path component type. *)
  type t =
    | PTrailing_slash : t
    | PFull_splat : t
    | PLiteral : string -> t
    | PArg : 'c Arg.t -> t

  let equal a b =
    match (a, b) with
    | PTrailing_slash, PTrailing_slash -> true
    | PFull_splat, PFull_splat -> true
    | PLiteral lit', PLiteral lit -> String.equal lit lit'
    | PArg arg', PArg arg -> (
      match Arg.eq arg.id arg'.id with
      | Some Arg.Eq -> true
      | None -> false)
    | _ -> false

  (* [of_path path] converts [path] to [kind list]. This is done to get around OCaml
     type inference issue when using [path] type in the [add] function below. *)
  let rec of_path : type a b. (a, b) path -> t list = function
    | Nil -> []
    | Trailing_slash -> [ PTrailing_slash ]
    | Full_splat -> [ PFull_splat ]
    | Literal (lit, path) -> PLiteral lit :: of_path path
    | Arg (arg, path) -> PArg arg :: of_path path
end

(** ['a t] is a node in a trie based router. *)
type 'a node =
  { route : 'a route option
  ; path : (Path_type.t * 'a node) list
  }

let rec add node (Route (path, _) as route) =
  let rec loop node = function
    | [] -> { node with route = Some route }
    | path_kind :: path_kinds ->
      List.find_opt
        (fun (path_kind', _) -> Path_type.equal path_kind path_kind')
        node.path
      |> (function
           | Some _ ->
             List.map
               (fun (path_kind', t') ->
                 if Path_type.equal path_kind path_kind' then
                   (path_kind', loop t' path_kinds)
                 else
                   (path_kind', t'))
               node.path
           | None -> (path_kind, loop empty path_kinds) :: node.path)
      |> update_path node
  in
  loop node (Path_type.of_path path)

and empty : 'a node = { route = None; path = [] }

and update_path t path = { t with path }

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

let rec match' t uri_path =
  let rec loop t decoded_values = function
    | [] ->
      Option.map
        (fun (Route (path, f)) ->
          exec_route_handler f (path, List.rev decoded_values))
        t.route
    | path_token :: path_tokens ->
      let continue = ref true in
      let index = ref 0 in
      let matched_node = ref None in
      let full_splat_matched = ref false in
      while !continue && !index < Array.length t.path do
        Path_type.(
          match t.path.(!index) with
          | PArg arg, t' -> (
            match arg.decode path_token with
            | Some v ->
              matched_node := Some (t', D (arg, v) :: decoded_values);
              continue := false
            | None -> incr index)
          | PLiteral lit, t' when String.equal lit path_token ->
            matched_node := Some (t', decoded_values);
            continue := false
          | PTrailing_slash, t' when String.equal "" path_token ->
            matched_node := Some (t', decoded_values);
            continue := false
          | PFull_splat, t' ->
            matched_node := Some (t', decoded_values);
            continue := false;
            full_splat_matched := true
          | _ -> incr index)
      done;
      Option.bind !matched_node (fun (t', decoded_values) ->
          if !full_splat_matched then
            (loop [@tailcall]) t' decoded_values []
          else
            (loop [@tailcall]) t' decoded_values path_tokens)
  in
  loop t [] (uri_tokens uri_path)

and uri_tokens s =
  let uri = Uri.of_string s in
  let path_tokens = Uri.path uri |> String.split_on_char '/' |> List.tl in
  Uri.query uri
  |> List.map (fun (k, v) ->
         if List.length v > 0 then
           [ k; List.hd v ]
         else
           [ k ])
  |> List.concat
  |> List.append path_tokens

and exec_route_handler : type a b. a -> (a, b) path * decoded_value list -> b =
 fun f -> function
  | Nil, [] -> f
  | Full_splat, [] -> f
  | Trailing_slash, [] -> f
  | Literal (_, path), decoded_values ->
    exec_route_handler f (path, decoded_values)
  | Arg ({ id; _ }, path), D ({ id = id'; _ }, v) :: decoded_values -> (
    match Arg.eq id id' with
    | Some Arg.Eq -> exec_route_handler (f v) (path, decoded_values)
    | None -> assert false)
  | _, _ -> assert false

module Private = struct
  let nil = Nil

  let trailing_slash = Trailing_slash

  let full_splat = Full_splat

  let lit s path = Literal (s, path)

  let arg a p = Arg (a, p)

  let int = Arg.int

  let float = Arg.float

  let bool = Arg.bool

  let string = Arg.string
end
