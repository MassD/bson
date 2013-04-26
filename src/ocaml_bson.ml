(* 
   Define the data structure of bson in ocaml.
   bson in ocaml is actually a Map whose key is string and value is element.
*)
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
  | Array of element list
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


(* create an emapty bson doc *)
let create_doc () = StringMap.empty;;

(*========================================================================================================*)
(*
   All put operations for bson.
  I write in this way because I believe this way is safer guard for bson types.
  However, anyway, direct element put is also included.
*)
let put ename e doc = StringMap.add ename e doc;;

let put_double ename v doc = put ename (Double v) doc;;
let put_string ename v doc = put ename (String v) doc;;
let put_doc ename v doc = put ename (Document v) doc;;
let put_list ename v doc = put ename (Array v) doc;;
let put_generic_binary ename v doc = put ename (Binary (Generic v)) doc;;
let put_function_binary ename v doc = put ename (Binary (Function v)) doc;;
let put_uuid_binary ename v doc = put ename (Binary (UUID v)) doc;;
let put_md5_binary ename v doc = put ename (Binary (MD5 v)) doc;;
let put_user_binary ename v doc = put ename (Binary (UserDefined v)) doc;;

exception Bson_invalid_objectId;;
let is_valid_objectId objectId = 
  if String.length objectId = 12 then true else false;;
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

(*========================================================================================================*)
(*
  The coresponding get operations.
*)
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
let get_list ename doc = get (function (Array v) -> v | _ -> raise Wrong_bson_type) ename doc;;
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


(*========================================================================================================*)
(*
  encode int64, int32 and float.
  note that encoding float is the same as int64, just need to transfer all the bits into an int64.

  The logic is that (e.g., for int32):
  1) we get an int32
  2) we shift right 1 byte one by one
  3) After each shift, we logic and 0000 0000 ... 0000 1111 1111 (255l) with the shifted int32 to get the lower 1 byte
  4) we convert the int32 to int, so Char.chr can pick it up and convert it to char (byte)
  5) we put the byte to the buffer (starting from index of 0, since it is little-endian format)
*)

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


(*========================================================================================================*)
(*
  encode the doc and element

  I intend to write the encoding/decoding as plain as possible. 
  There will be quite some redundant code but I guess in this way it is easier to read, especially with bson specification
*)

let encode doc =
  let add_ename c ename = 
    let buf = Buffer.create 16 in 
    Buffer.add_char buf c; Buffer.add_string buf ename; Buffer.add_char buf '\x00';
    buf
  in
  let add_string s =
    let buf = Buffer.create 16 in
    Buffer.add_buffer buf (encode_int32 (Int32.of_int ((String.length s) + 1)));
    Buffer.add_string buf s;
    Buffer.add_char buf '\x00';
    buf
  in 
  let add_binary c b = 
    let buf = Buffer.create 16 in
    Buffer.add_buffer buf (encode_int32 (Int32.of_int (Buffer.length b)));
    Buffer.add_char buf c;
    Buffer.add_buffer buf b;
    buf
  in 
  let rec encode_doc doc = 
    let encode_element ename element = 
      let buf = Buffer.create 16 in
      begin match element with
	| Double v -> 
	  Buffer.add_buffer buf (add_ename '\x01' ename);
	  Buffer.add_buffer buf (encode_float v)
	| String v -> 
	  Buffer.add_buffer buf (add_ename '\x02' ename);
	  Buffer.add_buffer buf (add_string v)
	| Document v -> 
	  Buffer.add_buffer buf (add_ename '\x03' ename);
	  Buffer.add_buffer buf (encode_doc v)
	| Array v ->
	  Buffer.add_buffer buf (add_ename '\x04' ename);
	  let rec trans_doc i acc = function (* we need to transform the list to a doc with key as incrementing from '0' *)
	    | [] -> acc
	    | hd::tl -> trans_doc (i+1) (put (String.make 1 (Char.chr (i+48))) hd acc) tl;
	  in 
	  let new_doc = trans_doc 0 (create_doc()) v in
	  Buffer.add_buffer buf (encode_doc new_doc)
	| Binary v ->
	  Buffer.add_buffer buf (add_ename '\x05' ename);	  
	  begin match v with
	    | Generic v -> Buffer.add_buffer buf (add_binary '\x00' v)
	    | Function v -> Buffer.add_buffer buf (add_binary '\x01' v)
	    | UUID v -> Buffer.add_buffer buf (add_binary '\x04' v)
	    | MD5 v -> Buffer.add_buffer buf (add_binary '\x05' v)
	    | UserDefined v -> Buffer.add_buffer buf (add_binary '\x80' v)
	  end 
	| ObjectId v -> 
	  Buffer.add_buffer buf (add_ename '\x07' ename);
	  Buffer.add_string buf v
	| Boolean v ->
	  Buffer.add_buffer buf (add_ename '\x08' ename);
	  Buffer.add_char buf (if v then '\x00' else '\x01')
	| UTC v ->
	  Buffer.add_buffer buf (add_ename '\x09' ename);
	  Buffer.add_buffer buf (encode_int64 v)
	| Null ->
	  Buffer.add_buffer buf (add_ename '\x0A' ename);
	| Regex (v1,v2) ->
	  Buffer.add_buffer buf (add_ename '\x0B' ename);
	  Buffer.add_string buf v1; Buffer.add_char buf '\x00';
	  Buffer.add_string buf v2; Buffer.add_char buf '\x00'
	| JSCode v ->
	  Buffer.add_buffer buf (add_ename '\x0D' ename);
	  Buffer.add_buffer buf (add_string v)
	| JSCodeWS (v, d) ->
	  Buffer.add_buffer buf (add_ename '\x0F' ename);
	  let doc_buf = encode_doc d in
	  Buffer.add_buffer buf (encode_int32 (Int32.of_int ((String.length v) + (Buffer.length doc_buf))));
	  Buffer.add_buffer buf (add_string v);
	  Buffer.add_buffer buf doc_buf
	| Int32 v -> 
	  Buffer.add_buffer buf (add_ename '\x10' ename);
	  Buffer.add_buffer buf (encode_int32 v)
	| Timestamp v -> 
	  Buffer.add_buffer buf (add_ename '\x11' ename);
	  Buffer.add_buffer buf (encode_int64 v)
	| Int64 v -> 
	  Buffer.add_buffer buf (add_ename '\x12' ename);
	  Buffer.add_buffer buf (encode_int64 v)
	| MinKey ->
	  Buffer.add_buffer buf (add_ename '\xFF' ename)
	| MaxKey ->
	  Buffer.add_buffer buf (add_ename '\x7F' ename)
      end;
      buf
    in 
    let bindings = StringMap.bindings doc in
    let process_element buf (ename, element) = Buffer.add_buffer buf (encode_element ename element); buf in
    let e_buf = List.fold_left process_element (Buffer.create 16) bindings in
    let d_buf = Buffer.create 16 in
    Buffer.add_buffer d_buf (encode_int32 (Int32.of_int (5+(Buffer.length e_buf))));
    print_string "e_buff len = "; print_int (Buffer.length e_buf); print_endline "";
    Buffer.add_buffer d_buf e_buf;
    Buffer.add_char d_buf '\x00';
    d_buf
  in 
  encode_doc doc;;
    
	  
