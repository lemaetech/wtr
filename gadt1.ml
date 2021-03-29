module Map = Map.Make (String)

type 'a decoder = string Map.t -> ('a, string) result

module Field = struct
  type 'a t =
    { name : string
    ; decoder : 'a decoder
    }

  type (_, _) list =
    | [] : ('a, 'a) list
    | ( :: ) : 'a t * ('b, 'c) list -> ('a -> 'b, 'c) list

  let make : string -> 'a decoder -> 'a t =
   fun name decoder -> { name; decoder }

  let bool : string -> 'a t =
   fun name ->
    let decoder map =
      match Map.find_opt name map with
      | Some v -> (
        try Ok (bool_of_string v) with
        | _ -> Error ("ReWeb.Form.Field.bool: " ^ name))
      | None -> Error "Not Found"
    in
    make name decoder
end

type ('ctor, 'ty) t =
  { fields : ('ctor, 'ty) Field.list
  ; ctor : 'ctor
  }

let decode : type ctor ty. (ctor, ty) t -> string Map.t -> (ty, string) result =
 fun fields form ->
  let rec loop :
      type ctor ty. (ctor, ty) t -> string list -> (ty, string) result =
   fun { fields; ctor } msg ->
    match fields with
    | Field.[] -> Ok ctor
    (* | Field.[ _ ] -> Error errors *)
    | Field.(field :: fields) -> (
      match field.decoder form with
      | Ok v -> (
        match ctor v with
        | ctor -> loop { fields; ctor } msg
        | exception ex ->
          let _errors = List.cons (Printexc.to_string ex) in
          loop { fields; ctor } msg)
      | Error _e -> failwith "")
  in
  loop fields []

let () = print_endline "Hello, World!"
