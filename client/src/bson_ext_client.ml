module type Bson_ext = sig
  type a
end

module Default(D : Bson_ext) : Bson_ext with type a = D.a = struct
  include D
end

module Bson_ext_int = Default(struct
    type a = int
  end)


module Bson_ext_int32 = Default(struct
    type a = int32
  end)

module Bson_ext_int64 = Default(struct
    type a = int64
  end)

module Bson_ext_bool = Default(struct
    type a = bool
  end)

module Bson_ext_float = Default(struct
    type a = float
  end)

module Bson_ext_string = Default(struct
    type a = string
  end)

module Bson_ext_list (A : Bson_ext) = Default(struct
    type a = A.a list
  end)


module Bson_ext_array (A : Bson_ext) = Default(struct
    type a = A.a array
  end)


module Bson_ext_option (A : Bson_ext) = Default(struct
    type a = A.a option
  end)
