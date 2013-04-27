open Ocaml_bson;;

let print_buffer buf = 
  let s = Buffer.contents buf in
  String.iter (fun c -> let k = Char.code c in if k < 16 then Printf.printf "\\x0%X" k else Printf.printf "\\x%X" k) s;
  print_endline "";;

let t_buf = Buffer.create 16;;
Buffer.add_char t_buf 'b';;

let doc0 = make();;


let e1 = create_double 1.0;;
let doc1 = add_element "Double" e1 doc0;;

let e2 = create_string "string";;
let doc2 = add_element "String" e2 doc1;;

let e3 = create_doc_element doc2;;
let doc3 = add_element "Document" e3 doc2;;

let e4 = create_array [e1;e2;e3];;
let doc4 = add_element "Array" e4 doc3;;

let e5 = create_generic_binary t_buf;;
let doc5 = add_element "Generic Binary" e5 doc4;;

let e6 = create_function_binary t_buf;;
let doc6 = add_element "Function Binary" e6 doc5

let e7 = create_uuid_binary t_buf;;
let doc7 = add_element "UUID Binary" e7 doc6;;

let e8 = create_md5_binary t_buf;;
let doc8 = add_element "MD5 Binary" e8 doc7;;

let e9 = create_user_binary t_buf;;
let doc9 = add_element "User Defined Binary" e9 doc8;;

let e10 = create_objectId "123456789123";;
let doc10 = add_element "ObjectId" e10 doc9;;

(*let e10_1 = create_objectId "00123456789123";;
let doc10_1 = add_element "ObjectId" e10_1 doc9;;*)

let e11 = create_bool true;;
let doc11 = add_element "Boolean" e11 doc10;;

let e12 = create_utc 123L;;
let doc12 = add_element "UTC" e12 doc11;;

let e13 = create_null ();;
let doc13 = add_element "Null" e13 doc12;;

let e14 = create_regex "pattern" "option";;
let doc14 = add_element "Regex" e14 doc13;;

let e15 = create_jscode "code...";;
let doc15 = add_element "JSCode" e15 doc14;;

let e16 = create_jscode_w_s "code" doc14;;
let doc16 = add_element "JSCodeWS" e16 doc15;;

let e17 = create_int32 123l;;
let doc17 = add_element "Int32" e17 doc16;;

let e18 = create_int64 123L;;
let doc18 = add_element "Int64" e18 doc17;;

let e19 = create_timestamp 123L;;
let doc19 = add_element "Timestamp" e19 doc18;;

let e20 = create_minkey ();;
let doc20 = add_element "MinKey" e20 doc19;;

let e21 = create_maxkey ();;
let doc21 = add_element "MaxKey" e21 doc20;;

let _ = print_endline "{\"hello\":\"world\"}";;
let e22 = create_string "world";;
let doc22 = add_element "hello" e22 doc0;;
let doc22_buf = encode doc22;;
let _ = print_buffer doc22_buf;;

print_endline "";;

let _ = print_endline "{\"BSON\":[\"awesome\", 5.05, 1986}";;
let l = [(create_string "awesome"); (create_double 5.05); (create_int32 1986l)];;
let e23 = create_array l;;
let doc23 = add_element "BSON" e23 doc0;;
let doc23_buf = encode doc23;;
let _ = print_buffer doc23_buf;;

print_endline "";;

let doc21_buf = encode doc21;;
let _ = print_buffer doc21_buf;;
