module type Bson_ext = sig
  type a
  val to_bson : a -> Bson.element
  val from_bson : Bson.element -> a
end

module Default(D : Bson_ext) : Bson_ext with type a = D.a = struct
  include D
end

module Bson_ext_int = Default(struct
    type a = int
    let from_bson i = Int64.to_int (Bson.get_int64 i)

    let to_bson i =
      Bson.create_int64 (Int64.of_int i)

  end)


module Bson_ext_int32 = Default(struct
    type a = int32

    let from_bson = Bson.get_int32
    let to_bson = Bson.create_int32

  end)

module Bson_ext_int64 = Default(struct
    type a = int64

    let from_bson = Bson.get_int64
    let to_bson = Bson.create_int64

  end)

module Bson_ext_bool = Default(struct
    type a = bool
    let from_bson = Bson.get_boolean
    let to_bson = Bson.create_boolean
  end)

module Bson_ext_float = Default(struct
    type a = float

    let from_bson = Bson.get_double
    let to_bson = Bson.create_double

  end)

module Bson_ext_string = Default(struct
    type a = string

    let from_bson = Bson.get_string
    let to_bson = Bson.create_string

  end)

module Bson_ext_list (A : Bson_ext) = Default(struct
    type a = A.a list
    let from_bson elt =
      List.map (
        fun bson ->
          A.from_bson bson
      ) (Bson.get_list elt)

    let to_bson l =
      Bson.create_list (List.map (fun e -> A.to_bson e) l)

  end)


module Bson_ext_array (A : Bson_ext) = Default(struct
    type a = A.a array
    let from_bson elt =
      let l =
        List.map (
          fun bson ->
            A.from_bson bson
        ) (Bson.get_list elt)
      in

      Array.of_list l

    let to_bson a =
      let l = Array.to_list a in
      Bson.create_list (List.map (fun e -> A.to_bson e) l)
  end)


module Bson_ext_option (A : Bson_ext) = Default(struct
    type a = A.a option
    let from_bson o =
      try
        let _ = Bson.get_null o in
        None
      with Bson.Wrong_bson_type ->
        Some (A.from_bson o)

    let to_bson = function
      | None -> Bson.create_null ()
      | Some o -> A.to_bson o

  end)
