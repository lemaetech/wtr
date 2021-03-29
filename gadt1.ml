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

let decode fields form =
  let rec loop :
      type ctor ty. (ctor, ty) t -> string list -> (ty, string list) result =
   fun ffields errors ->
    let open! Field in
    match ffields.fields with
    | [] ->
      if List.length errors > 0 then
        Error errors
      else
        Ok ffields.ctor
    | field :: tl -> (
      match field.decoder form with
      | Ok v -> (
        match ffields.ctor v with
        | ctor -> loop { fields = tl; ctor } errors
        | exception ex ->
          let errors = List.cons (Printexc.to_string ex) errors in
          loop { ffields with fields = tl } errors)
      | Error e ->
        let errors = List.cons e errors in
        loop { ffields with fields = tl } errors)
  in
  loop fields []

let () = print_endline "Hello, World!"
