open Camlp4
open Pa_deriving_common
open Utils

module Id : Sig.Id =
struct
  let name = "pa_bson"
  let version = "0.1"
end

module Description : Defs.ClassDescription = struct
  let classname = "Bson_ext"
  let runtimename = "Bson_ext_client"
  let default_module = None
  let alpha = None
  let allow_private = false
  let predefs = [
    ["int"], ["Bson_ext_client";"int"];
    ["int32"], ["Bson_ext_client";"int32"];
    ["Int32";"t"], ["Bson_ext_client";"int32"];
    ["int64"], ["Bson_ext_client";"int64"];
    ["Int64";"t"], ["Bson_ext_client";"int64"];
    ["bool"], ["Bson_ext_client";"bool"];
    ["float"], ["Bson_ext_client";"float"];
    ["string"], ["Bson_ext_client";"string"];
    ["list"], ["Bson_ext_client";"list"];
    ["array"],["Bson_ext_client";"array"];
    ["option"], ["Bson_ext_client";"option"];
  ]


  let depends = []
end

module Builder(Loc : Defs.Loc) = struct

  module Helpers = Base.AstHelpers(Loc)
  module Generator = Base.Generator(Loc)(Description)

  open Loc
  open Camlp4.PreCast
  open Description

  let generator = (object(self)

  inherit Generator.generator

    method proxy () =
      None, [
      ]

    method record ?eq ctxt tname params constraints (fields : Pa_deriving_common.Type.field list) =
      [
        <:str_item< value f _ =  assert False >>
      ]

    method tuple ctxt tys =
      [
        <:str_item< value f _ =  assert False >>
      ]

    method sum ?eq ctxt tname params constraints summands =
      [
        <:str_item< value f _ =  assert False >>
      ]


    method variant ctxt tname params constraints (_, tags) =
      [
        <:str_item< value f _ =  assert False >>
      ]


end :> Generator.generator)

let generate = Generator.generate generator
let generate_sigs = Generator.generate_sigs generator

end

module Bson_ext = Base.Register(Description)(Builder)
