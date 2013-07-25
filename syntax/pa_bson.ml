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
  let runtimename = "Bson_ext"
  let default_module = None
  let alpha = None
  let allow_private = false
  let predefs = [
    ["int"], ["Bson_ext";"int"];
    ["int32"], ["Bson_ext";"int32"];
    ["Int32";"t"], ["Bson_ext";"int32"];
    ["int64"], ["Bson_ext";"int64"];
    ["Int64";"t"], ["Bson_ext";"int64"];
    ["bool"], ["Bson_ext";"bool"];
    ["float"], ["Bson_ext";"float"];
    ["string"], ["Bson_ext";"string"];
    ["list"], ["Bson_ext";"list"];
    ["array"],["Bson_ext";"array"];
    ["option"], ["Bson_ext";"option"];
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
      None, [ <:ident< to_bson >>;
	            <:ident< from_bson >>;
            ]

    method record ?eq ctxt tname params constraints (fields : Pa_deriving_common.Type.field list) =
      let to_bson =
        let exprs =
          List.map (
            fun (name,ty,_) ->
              <:expr<
                  !bson_doc := Bson.add_element $`str:name$ ($self#call_poly_expr ctxt ty "to_bson"$ t.$lid:name$) !bson_doc
              >>
          ) fields
        in

        <:str_item<
          value to_bson (t : $lid:tname$) =
            let bson_doc = ref Bson.empty in
            do {
               $Ast.exSem_of_list exprs$;
               Bson.create_doc_element !bson_doc
            };
        >>
      in

      let from_bson =
        let l =
          List.map (
            fun (name,ty,_) ->
              let expr =
                <:expr<
                  let b = Bson.get_element $`str:name$ bson_doc in
                  $self#call_poly_expr ctxt ty "from_bson"$ b
                >>
              in
              name,expr
          ) fields
        in

        <:str_item<
          value from_bson bson : $lid:tname$ =
            let bson_doc = Bson.get_doc_element bson in
            $Helpers.record_expr l$
        >>
      in

      [
        to_bson;
        from_bson
      ]

    method tuple ctxt tys =
      let ntys = List.length tys in
      let ids,tpatt,texpr = Helpers.tuple ntys in

      let to_bson =
        let l = List.map (
            fun (ty,id) ->
              <:expr<
                (* $lid:id$ variable are define by the $tpatt$ below, They are normal type *)
                $self#call_expr ctxt ty "to_bson"$ $lid:id$
              >>
          ) (List.zip tys ids)
        in

        <:str_item<
          value to_bson t =
            (* $tpatt$ will be something like (id1,id2,id3...), so here id are value name of a subset of t*)
            let $tpatt$ = t in
            Bson.create_list ($Helpers.expr_list l$)
        >>
      in

      let from_bson =
        let l =
          List.map (
            fun (ty,id) ->
              (* $lid:id$ variable are define by the match case below, they are bson type *)
              <:expr< $self#call_expr ctxt ty "from_bson"$ $lid:id$ >>
          ) (List.zip tys ids)
        in

        <:str_item<
          value from_bson bson =
            match (Bson.get_list bson) with
             [ $Helpers.patt_list (List.map (fun x -> <:patt<$lid:x$>>) ids)$ -> $Helpers.tuple_expr l$
                  | _ -> raise Bson.Wrong_bson_type
             ]
        >>
      in

      [
        to_bson;
        from_bson
      ]

    method sum_to_bson: 'c. Generator.context -> ('a -> 'b -> 'c -> 'd) -> 'c list -> Camlp4.PreCast.Ast.str_item = fun ctxt mc tags ->
      let to_bson =
        let no_expr ~is_sum name =
          if is_sum then
            <:match_case<
              $uid:name$ ->
                let d = Bson.add_element $str:name$ (Bson.create_null ()) Bson.empty in
                Bson.create_doc_element d
            >>
          else
            <:match_case<
              `$uid:name$ ->
                let d = Bson.add_element $str:name$ (Bson.create_null ()) Bson.empty in
                Bson.create_doc_element d
            >>
        in

        let with_expr ~is_sum name tys =
          let ntys = List.length tys in
          let ids,tpatt,texpr = Helpers.tuple ntys in

          let l = List.map (
              fun (ty,id) ->
                <:expr<
                  (* $lid:id$ variable are define by the $tpatt$ below, They are normal type *)
                  $self#call_expr ctxt ty "to_bson"$ $lid:id$
                >>
            ) (List.zip tys ids)
          in

          if is_sum then
            <:match_case<
              $uid:name$ $tpatt$ ->
                let l = Bson.create_list $Helpers.expr_list l$ in
                let d = Bson.add_element $str:name$ l Bson.empty in
                Bson.create_doc_element d
            >>
          else
            <:match_case<
              `$uid:name$ $tpatt$ ->
                let l = Bson.create_list $Helpers.expr_list l$ in
                let d = Bson.add_element $str:name$ l Bson.empty in
                Bson.create_doc_element d
            >>
        in

        let mcs = List.map (mc no_expr with_expr) tags in

        <:str_item<
          value to_bson t =
            match t with
              [
                $list:mcs$
              ]
        >>
      in

      to_bson

    method sum_from_bson: 'c. Generator.context -> ('e -> 'f -> 'c -> 'g) -> 'c list -> Camlp4.PreCast.Ast.str_item = fun ctxt mc tags ->
      let from_bson =
        let no_expr acc ~is_sum name =
          if is_sum then
            <:match_case<
              $str:name$ ->
                $uid:name$
            >>::acc
          else
            <:match_case<
              $str:name$ ->
                `$uid:name$
            >>::acc
        in

        let with_expr acc ~is_sum name tys =
          let ntys = List.length tys in
          let ids,tpatt,texpr = Helpers.tuple ntys in

          let l =
            List.map (
              fun (ty,id) ->
                (* $lid:id$ variable are define by the match case below, they are bson type *)
                <:expr< $self#call_expr ctxt ty "from_bson"$ $lid:id$ >>
            ) (List.zip tys ids)
          in

          if is_sum then
            <:match_case<
              $str:name$ ->
                match Bson.get_list el with
                 [ $Helpers.patt_list (List.map (fun x -> <:patt<$lid:x$>>) ids)$ ->
                     $uid:name$ $Helpers.tuple_expr l$
                   | _ -> raise Bson.Wrong_bson_type
                 ]
            >>::acc
          else
            <:match_case<
              $str:name$ ->
                match Bson.get_list el with
                 [ $Helpers.patt_list (List.map (fun x -> <:patt<$lid:x$>>) ids)$ ->
                     `$uid:name$ $Helpers.tuple_expr l$
                   | _ -> raise Bson.Wrong_bson_type
                 ]
            >>::acc
        in

        let mcs =
          List.fold_left (
            fun acc t ->
              mc (no_expr acc) (with_expr acc) t
          ) [ <:match_case< _ -> raise Bson.Wrong_bson_type >> ] tags
        in

        <:str_item<
          value from_bson bson =
            let d = Bson.get_doc_element bson in
            let els = Bson.all_elements d in
            let (key,el) = List.nth els 0 in

            match key with
              [
                $list:mcs$
              ]
        >>
     in

     from_bson



    method sum ?eq ctxt tname params constraints summands =
      let mc no_expr with_expr =
        function
          | (name, []) -> no_expr ~is_sum:true name
          | (name,tys) -> with_expr ~is_sum:true name tys
      in

      let to_bson = self#sum_to_bson ctxt mc summands in
      let from_bson = self#sum_from_bson ctxt mc summands in

      [
        to_bson ;
        from_bson ;
      ]


    method variant ctxt tname params constraints (_, tags) =
      [
        <:str_item< value f _ =  assert False >>
      ]

    method variant ctxt tname params constraints (_, tags) =
      let mc no_expr with_expr =
        function
          | Type.Tag (name, []) -> no_expr ~is_sum:false name
          | Type.Tag (name,tys) -> with_expr ~is_sum:false name tys
          | Type.Extends _ -> assert false
      in

      let to_bson = self#sum_to_bson ctxt mc tags in
      let from_bson = self#sum_from_bson ctxt mc tags in

      [
        to_bson ;
        from_bson ;
      ]


  end :> Generator.generator)

  let generate decls =
    let i = Generator.generate generator decls in

    let modules =
      List.map (
        fun t ->
          let (name,_,_,_,_) = t in
          <:str_item<
            module $uid:("Bson_utils_" ^ name)$ =
            struct
              value to_bson t =
                let elt = $uid:("Bson_ext_" ^ name)$.to_bson t in
                Bson.get_doc_element elt;

              value from_bson b =
                let elt = Bson.create_doc_element b in
                $uid:("Bson_ext_" ^ name)$.from_bson elt;

            end
          >>
      ) decls
    in

    <:str_item<
      $i$;
      $list:modules$
    >>


  let generate_sigs = Generator.generate_sigs generator

end

module Bson_ext = Base.Register(Description)(Builder)
