module StringMap = Map.Make(struct type t = string let compare = compare end);;

type
  document = element StringMap.t
and
  byte = char
and
  regex = string * string
and
  jscode_w_s = string * document
and 
  element = 
  | Double of float
  | String of string
  | Document of document
  | Array of document array
  | Binary of binary
  | ObjectId of string (* only 12 bytes *)
  | Boolean of bool
  | UTC of int64
  | Null
  | Regex of regex
  | JSCode of string
  | JSCodeWS of jscode_w_s
  | Int32 of int32
  | Int64 of int64
  | Timestamp of int64
  | MinKey
  | MaxKey
and
  binary =
  | Generic of byte array
  | Function of byte array
  | UUID of byte array
  | MD5 of byte array
  | UserDefined of byte array;;

exception Bson_invalid_objectId;;

let is_valid_objectId objectId = 
  if String.length objectId = 12 then true else false;;

let create_doc () = StringMap.empty;;

let put ename e doc = StringMap.add ename e doc;;

let put_double ename v doc = put ename (Double v) doc;;
let put_string ename v doc = put ename (String v) doc;;
let put_doc ename v doc = put ename (Document v) doc;;
let put_array ename v doc = put ename (Array v) doc;;
let put_generic_binary ename v doc = put ename (Binary (Generic v)) doc;;
let put_function_binary ename v doc = put ename (Binary (Function v)) doc;;
let put_uuid_binary ename v doc = put ename (Binary (UUID v)) doc;;
let put_md5_binary ename v doc = put ename (Binary (MD5 v)) doc;;
let put_user_binary ename v doc = put ename (Binary (UserDefined v)) doc;;
let put_objectId ename v doc = 
  if is_valid_objectId v then put ename (ObjectId v) doc
  else raise Bson_invalid_objectId;;
let put_bool ename v doc = put ename (Boolean v) doc;;
let put_utc ename v doc = put ename (UTC v) doc;;
let put_null ename doc = put ename Null doc;;
let put_regex ename v doc = put ename (Regex v) doc;;
let put_jscode ename v doc = put ename (JSCode v) doc;;
let put_jscode_w_s ename v doc = put ename (JSCodeWS v) doc;;
let put_int32 ename v doc = put ename (Int32 v) doc;;
let put_int64 ename v doc = put ename (Int64 v) doc;;
let put_timestamp ename v doc = put ename (Timestamp v) doc;;
let put_minkey ename doc = put ename MinKey doc;;
let put_maxkey ename doc = put ename MaxKey doc;;

let put_element ename element doc = put ename element doc;;

exception Wrong_bson_type;;

let get_element ename doc = try Some (StringMap.find ename doc) with Not_found -> None;;

let get extract ename doc = 
  let element = get_element ename doc  in
  match element with
    | None -> None
    | Some e -> Some (extract e);;

let get_double ename doc = get (function (Double v) -> v | _ -> raise Wrong_bson_type) ename doc;;
let get_string ename doc = get (function (String v) -> v | _ -> raise Wrong_bson_type) ename doc;;
let get_doc ename doc = get (function (Document v) -> v | _ -> raise Wrong_bson_type) ename doc;;
let get_array ename doc = get (function (Array v) -> v | _ -> raise Wrong_bson_type) ename doc;;
let get_generic_binary ename doc = get (function (Generic v) -> v | _ -> raise Wrong_bson_type) ename doc;;
let get_function_binary ename doc = get (function (Function v) -> v | _ -> raise Wrong_bson_type) ename doc;;
let get_uuid_binary ename doc = get (function (UUID v) -> v | _ -> raise Wrong_bson_type) ename doc;;
let get_md5_binary ename doc = get (function (MD5 v) -> v | _ -> raise Wrong_bson_type) ename doc;;
let get_user_binary ename doc = get (function (UserDefined v) -> v | _ -> raise Wrong_bson_type) ename doc;;
let get_objectId ename doc = get (function (ObjectId v) -> v | _ -> raise Wrong_bson_type) ename doc;;
let get_bool ename doc = get (function (Boolean v) -> v | _ -> raise Wrong_bson_type) ename doc;;
let get_utc ename doc = get (function (UTC v) -> v | _ -> raise Wrong_bson_type) ename doc;;
let get_null ename doc = get (function Null -> Null | _ -> raise Wrong_bson_type) ename doc;;
let get_regex ename doc = get (function (Regex v) -> v | _ -> raise Wrong_bson_type) ename doc;;
let get_jscode ename doc = get (function (JSCode v) -> v | _ -> raise Wrong_bson_type) ename doc;;
let get_jscode_w_s ename doc = get (function (JSCodeWS v) -> v | _ -> raise Wrong_bson_type) ename doc;;
let get_int32 ename doc = get (function (Int32 v) -> v | _ -> raise Wrong_bson_type) ename doc;;
let get_int64 ename doc = get (function (Int64 v) -> v | _ -> raise Wrong_bson_type) ename doc;;
let get_timestamp ename doc = get (function (Timestamp v) -> v | _ -> raise Wrong_bson_type) ename doc;;
let get_minkey ename doc = get (function MinKey -> MinKey | _ -> raise Wrong_bson_type) ename doc;;
let get_maxkey ename doc = get (function MaxKey -> MaxKey | _ -> raise Wrong_bson_type) ename doc;;

let encode_int64 v =
  let buf = Buffer.create 8 in
  for i = 0 to 7 do 
    Buffer.add_char buf (Int64.logand 255L (Int64.shift_right v (i*8))) 
  done;;

let encode_float v = encode_int64 (Int64.bits_of_float v);;

 

(*
let encode doc =
  let rec encode_doc = 
    let encode_element ename element = 
      let append_end buf = Buffer.add_char buf '\x00' in
      let buf = Buffer.create 16 in
      match element with
	| Double v -> 
	  Buffer.add_char buf '\x01'; Buffer.add_string buf ename; append_end buf; 
	| String -> *)
