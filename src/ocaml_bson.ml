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
  | Array of document list
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
  | Generic of Buffer.t
  | Function of Buffer.t
  | UUID of Buffer.t
  | MD5 of Buffer.t
  | UserDefined of Buffer.t;;

exception Bson_invalid_objectId;;

let is_valid_objectId objectId = 
  if String.length objectId = 12 then true else false;;

let create_doc () = StringMap.empty;;

let put ename e doc = StringMap.add ename e doc;;

let put_double ename v doc = put ename (Double v) doc;;
let put_string ename v doc = put ename (String v) doc;;
let put_doc ename v doc = put ename (Document v) doc;;
let put_list ename v doc = 
  let rec build_doc_list i acc = function 
    | [] -> acc 
    | hd::tl -> 
      let new_doc = put (String.make 1 (Char.chr (i+48))) hd (create_doc()) in
      build_doc_list (i+1) (new_doc::acc) tl 
  in 
  put ename (List.rev (build_doc_list 0 [] v)) doc;;
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
let get_list ename doc = 
  let doc_list = get (function (Array v) -> v | _ -> raise Wrong_bson_type) ename doc in
  match doc_list with
    | None -> None
    | Some dl -> 
      let rec build_element_list i acc = function
	| [] -> acc
	| hd::tl ->
	  let element = StringMap.find (String.make 1 (Char.chr (i+48))) hd in
	  build_element_list (i+1) (element::acc) tl
      in 
      Some (List.rev (build_element_list 0 [] dl));;
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

(*let encode_int64 v =
  let rec convert i acc = 
    if i > 7 then acc
    else 
      let b = Int64.logand 255L (Int64.shift_right v (i*8)) in
      let acc_move = Int64.shift_left acc (i*8) in
      convert (i+1) (Int64.logor acc_move b)
  in 
  convert 0 0L;;*)

let encode_int64 v =
  let buf = Buffer.create 8 in
  for i = 0 to 7 do
    let b = Int64.logand 255L (Int64.shift_right v (i*8)) in
    Buffer.add_char buf (Char.chr (Int64.to_int b)) ;
  done;
  buf;;

let encode_float v = encode_int64 (Int64.bits_of_float v);;

let encode_int32 v = 
  let buf = Buffer.create 4 in
  for i = 0 to 3 do
    let b = Int32.logand 255l (Int32.shift_right v (i*8)) in
    Buffer.add_char buf (Char.chr (Int32.to_int b)) ;
  done;
  buf;;

(*
let encode doc =
  let add_ename c ename = 
    let buf = Buffer.create 16 in 
    Buffer.add_char buf c; Buffer.add_string buf ename; Buffer.add_char buf '\x00';
    buf
  in 
  let rec encode_doc doc = 
    let encode_element ename element = 
      let buf = Buffer.create 16 in
      match element with
	| Double v -> 
	  Buffer.add_buffer buf (add_ename '\x01' ename);
	  Buffer.add_buffer buf (encode_float v)
	| String v -> 
	  Buffer.add_buffer buf (add_ename '\x02' ename);
	  Buffer.add_buffer buf (encode_int32 (Int32.of_int (String.length v)));
	  Buffer.add_string buf v;
	  Buffer.add_char '\x00'
	| Document v -> 
	  Buffer.add_buffer buf (add_ename '\x03' ename);
	  Buffer.add_buffer buf (encode_doc v)
	| Array v ->
	  Buffer.add_buffer buf (add_ename '\x04' ename);
	  for i = 0 to ((Array.length v)-1) do
	    Buffer.add_buffer buf (encode_element (Char.chr (i+48)) v.(i))
	  done
	| Binary (Generic v) ->
	  Buffer.add_buffer buf (add_ename '\x05' ename);*)
