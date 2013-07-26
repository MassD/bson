let print_buffer s =
  String.iter (fun c -> let i = Char.code c in if i < 15 then Printf.printf "\\x0%X" i else Printf.printf "\\x%X" i) s;
  print_endline "";;

let bin = "12345";;

let doc0 = Bson.empty;;

let e1 = Bson.create_double 1.0;;
let doc1 = Bson.add_element "Double" e1 doc0;;

let e2 = Bson.create_string "string";;
let doc2 = Bson.add_element "String" e2 doc1;;


let e2_1 = Bson.create_string "";;
let doc2_1 = Bson.add_element "String" e2 doc1;;

let e3 = Bson.create_doc_element doc2;;
let doc3 = Bson.add_element "Document" e3 doc2;;

let e4 = Bson.create_list [e1;e2;e3];;
let doc4 = Bson.add_element "Array" e4 doc3;;

let e9 = Bson.create_user_binary bin;;
let doc9 = Bson.add_element "User Defined Binary" e9 doc4;;

let e10 = Bson.create_objectId "123456789123";;
let doc10 = Bson.add_element "ObjectId" e10 doc9;;

let e11 = Bson.create_boolean true;;
let doc11 = Bson.add_element "Boolean" e11 doc10;;

let e12 = Bson.create_utc 123L;;
let doc12 = Bson.add_element "UTC" e12 doc11;;

let e13 = Bson.create_null ();;
let doc13 = Bson.add_element "Null" e13 doc12;;

let e14 = Bson.create_regex "pattern" "option";;
let doc14 = Bson.add_element "Regex" e14 doc13;;

let e15 = Bson.create_jscode "code...";;
let doc15 = Bson.add_element "JSCode" e15 doc14;;

let e16 = Bson.create_jscode_w_s "code" doc14;;
let doc16 = Bson.add_element "JSCodeWS" e16 doc15;;

let e17 = Bson.create_int32 123l;;
let doc17 = Bson.add_element "Int32" e17 doc16;;

let e18 = Bson.create_int64 123L;;
let doc18 = Bson.add_element "Int64" e18 doc17;;

(*let e19 = Bson.create_timestamp 123L;;
let doc19 = Bson.add_element "Timestamp" e19 doc18;;*)

let e20 = Bson.create_minkey ();;
let doc20 = Bson.add_element "MinKey" e20 doc18;;

let e21 = Bson.create_maxkey ();;
let doc21 = Bson.add_element "MaxKey" e21 doc20;;

let e22 = Bson.create_string "world";;
let doc22 = Bson.add_element "hello" e22 doc0;;
let _ = print_endline (Bson.to_simple_json doc22);;

let _ = print_endline "encoding...";;
let doc22_buf = Bson.encode doc22;;
let _ = print_buffer doc22_buf;;

let _ = print_endline "decoding...";;
let doc22_rev = Bson.decode doc22_buf;;
let _ = print_endline (Bson.to_simple_json doc22_rev);;

print_endline "";;

let l = [(Bson.create_string "awesome"); (Bson.create_double 5.05); (Bson.create_int32 1986l)];;
let e23 = Bson.create_list l;;
let doc23 = Bson.add_element "BSON" e23 doc0;;
let _ = print_endline (Bson.to_simple_json doc23);;

let _ = print_endline "encoding...";;
let doc23_buf = Bson.encode doc23;;
let _ = print_buffer doc23_buf;;

let _ = print_endline "decoding...";;
let doc23_rev = Bson.decode doc23_buf;;
let _ = print_endline (Bson.to_simple_json doc23_rev);;

print_endline "";;

let _ = print_endline "Testing Bson_ext...";;

type variants =
  | Var1 of int
  | Var2
  | Var3 of (string * int)
    deriving (Bson_ext)

type variants_poly = [
  | `VarP1 of int
  | `VarP2
  | `VarP3 of (string * int)
 ] deriving (Bson_ext)


type test_mongo = {
  str : string ;
  i : int ;
  i64 : int64 ;
  b : bool ;
  l : int list ;
  o : int option ;
  t : int * string * float;
  v : variants;
  vp : variants_poly;
} deriving (Bson_ext)

let _ =
  let t = {
    str = "test";
    i = 5 ;
    i64 = 85L ;
    b = false ;
    l = [ 3; 4 ; 5 ; 8 ] ;
    o = Some 5 ;
    t = (7, "seven", 7.);
    v = Var1 5;
    vp = `VarP3 ("Varp3", 3)
  } in

  let d = Bson_utils_test_mongo.to_bson t in
  let t_ = Bson_utils_test_mongo.from_bson d in

  if t_.str <> t.str then print_endline "Bson_ext: error on string type";
  if t_.i <> t.i then print_endline "Bson_ext: error on int type";
  if t_.i64 <> t.i64 then print_endline "Bson_ext: error on int64 type";
  if t_.t <> t.t then print_endline "Bson_ext: error on tuple type";

  List.iter2 (
    fun e_ e ->
      if e_ <> e then print_endline "Bson_ext: error on list type"
  ) t_.l t.l;

  begin match t_.o, t.o with
    | None,None -> ()
    | Some o_, Some o when o_ = o -> ()
    | _ -> print_endline "Bson_ext: error on list type"
  end;

  begin match t_.v, t.v with
    | Var1 i_, Var1 i when i_ = i -> ()
    | Var2, Var2 -> ()
    | Var3 (s_,i_), Var3 (s,i) when i_ = i && s_ = s -> ()
    | _ -> print_endline "Bson_ext: error on variant type"
  end;

  begin match t_.vp, t.vp with
    | `VarP1 i_, `VarP1 i when i_ = i -> ()
    | `VarP2, `VarP2 -> ()
    | `VarP3 (s_,i_), `VarP3 (s,i) when i_ = i && s_ = s -> ()
    | _ -> print_endline "Bson_ext: error on polymorphic variant type"
  end;

  print_endline "... tests successful"


(*
  Debugging for Issue `Cannot decode string in which there are `\x00`s`
*)
(*
let _ = print_buffer "x86_64";;


let s = "FF0200000373797374656D00800000000963757272656E7454696D6500AAC591953E01000002686F73746E616D650007000000286E756C6C2900106370754164647253697A650040000000106D656D53697A654D420000100000106E756D436F7265730002000000026370754172636800080000007838365F36340000086E756D61456E61626C6564000000036F73003E0000000274797065000700000044617277696E00026E616D6500090000004D6163204F532058000276657273696F6E000800000031322E332E30000000036578747261001D0200000276657273696F6E537472696E67006200000044617277696E204B65726E656C2056657273696F6E2031322E332E303A2053756E204A616E2020362032323A33373A31302050535420323031333B20726F6F743A786E752D323035302E32322E31337E312F52454C454153455F5838365F3634000010616C7761797346756C6C53796E630000000000106E66734173796E630000000000026D6F64656C000F0000004D6163426F6F6B50726F332C31000010706879736963616C436F7265730002000000106370754672657175656E63794D487A006009000002637075537472696E670031000000496E74656C28522920436F726528544D29322044756F204350552020202020543737303020204020322E343047487A000002637075466561747572657300C400000046505520564D452044452050534520545343204D535220504145204D434520435838204150494320534550204D54525220504745204D434120434D4F562050415420505345333620434C4653482044532041435049204D4D5820465853522053534520535345322053532048545420544D20504245205353453320445445533634204D4F4E20445343504C20564D582045535420544D32205353534533204358313620545052205044434D002053595343414C4C20584420454D363454204C4148460000107061676553697A650000100000027363686564756C6572000D000000747261646974696F6E616C000000016F6B00000000000000F03F00";;

let hex_to_char s =
  let rec cal i h acc =
    if h < 0 then acc
    else
      let code = Char.code s.[h] in
      let n =
	if code < 58 then code-48
	else  code-55
      in
      cal (i*16) (h-1) (acc + i*n)
  in
  Char.chr (cal 1 ((String.length s) - 1) 0);;

let s_buf s =
  let buf = Buffer.create ((String.length s) / 2) in
  let rec convert cur =
    if cur < String.length s then
      let one_char = String.sub s cur 2 in
      Buffer.add_char buf (hex_to_char one_char);
      convert (cur+2)
  in
  convert 0;
  Buffer.contents buf;;

let bson_buf = s_buf s;;
let bson_buf_doc = Bson.decode bson_buf;;
let _ = print_endline (Bson.to_simple_json bson_buf_doc);;
*)
