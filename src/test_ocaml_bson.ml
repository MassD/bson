open Ocaml_bson;;

let t_buf = Buffer.create 16;;
Buffer.add_char t_buf 'b';;

let doc0 = create_doc ();;
let doc1 = put_double "double" 1.0 doc0;;
let doc2 = put_string "string" "string" doc1;;
let doc3 = put_doc "doc" (create_doc ()) doc2;;
let doc4 = put_list "array" [(String "hello");(String "world")]  doc3;;
let doc5 = put_generic_binary "generic" t_buf doc4;;
let doc6 = put_function_binary "function" t_buf doc5;;
let doc7 = put_uuid_binary "uuid" t_buf doc6;;
let doc8 = put_md5_binary "md5" t_buf doc7;;
let doc9 = put_user_binary "user" t_buf doc8;;
let doc10 = put_objectId "objectId" "123456789123" doc9;;
let doc11 = put_bool "boolean" true doc10;;
let doc12 = put_utc "utc" 123L doc11;;
let doc13 = put_null "null" doc12;;
let doc14 = put_regex "regex" ("pattern", "option") doc13;;
let doc15 = put_jscode "jscode" "code..." doc14;;
let doc16 = put_jscode_w_s "jscode_w_s" ("code", create_doc()) doc15;;
let doc17 = put_int32 "int32" 123l doc16;;
let doc18 = put_int64 "int64" 123L doc17;;
let doc19 = put_timestamp "timestamp" 123L doc18;;
let doc20 = put_minkey "minkey" doc19;;
let doc21 = put_maxkey "maxkey" doc20;;

let b = encode_float 5.05;;
let doc22 = put_string "hello" "world" doc0;;
let doc22_buf = encode doc22;
