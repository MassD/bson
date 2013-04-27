(* 
   Define the data structure of bson in ocaml.
   bson in ocaml is actually a Map whose key is string and value is element.
*)
module StringMap = Map.Make(struct type t = string let compare = compare end);;

type
  document = element StringMap.t
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
  | Regex of (string * string)
  | JSCode of string
  | JSCodeWS of (string * document)
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
let make () = StringMap.empty;;

(*========================================================================================================*)
(*
   All put operations for bson.
  I write in this way because I believe this way is safer guard for bson types.
  However, anyway, direct element put is also included.
*)
let add_element ename element doc = StringMap.add ename element doc;;

let create_double v = Double v;;
let create_string v = String v;;
let create_doc_element v = Document v;;
let create_array element_list = Array element_list;;

let create_generic_binary v = Binary (Generic v);;
let create_function_binary v = Binary (Function v);;
let create_uuid_binary v = Binary (UUID v);;
let create_md5_binary v = Binary (MD5 v);;
let create_user_binary v = Binary (UserDefined v);;

exception Bson_invalid_objectId;;
let is_valid_objectId objectId = if String.length objectId = 12 then true else false;;
let create_objectId v = 
  if is_valid_objectId v then ObjectId v
  else raise Bson_invalid_objectId;;

let create_bool v = Boolean v;;
let create_utc v = UTC v;;
let create_null () = Null;;
let create_regex s1 s2 = Regex (s1, s2);;
let create_jscode v = JSCode v;;
let create_jscode_w_s s doc = JSCodeWS (s, doc);;
let create_int32 v = Int32 v;;
let create_int64 v = Int64 v;;
let create_timestamp v = Timestamp v;;
let create_minkey () = MinKey;;
let create_maxkey () = MaxKey;;


(*========================================================================================================*)
(*
  The coresponding get operations.
*)
exception Wrong_bson_type;;

let get_element ename doc = StringMap.find ename doc;;

let get_double = function | Double v -> v | _ -> raise Wrong_bson_type;;
let get_string = function | String v -> v | _ -> raise Wrong_bson_type;;
let get_doc_element = function | Document v -> v | _ -> raise Wrong_bson_type;;
let get_list = function | Array v -> v | _ -> raise Wrong_bson_type;;
let get_generic_binary = function | Generic v -> v | _ -> raise Wrong_bson_type;;
let get_function_binary = function | Function v -> v | _ -> raise Wrong_bson_type;;
let get_uuid_binary = function | UUID v -> v | _ -> raise Wrong_bson_type;;
let get_md5_binary = function | MD5 v -> v | _ -> raise Wrong_bson_type;;
let get_user_binary = function | UserDefined v -> v | _ -> raise Wrong_bson_type;;
let get_objectId = function | ObjectId v -> v | _ -> raise Wrong_bson_type;;
let get_bool = function | Boolean v -> v | _ -> raise Wrong_bson_type;;
let get_utc = function | UTC v -> v | _ -> raise Wrong_bson_type;;
let get_null = function | Null -> Null | _ -> raise Wrong_bson_type;;
let get_regex = function | Regex v -> v | _ -> raise Wrong_bson_type;;
let get_jscode = function | JSCode v -> v | _ -> raise Wrong_bson_type;;
let get_jscode_w_s = function | JSCodeWS v -> v | _ -> raise Wrong_bson_type;;
let get_int32 = function | Int32 v -> v | _ -> raise Wrong_bson_type;;
let get_int64 = function | Int64 v -> v | _ -> raise Wrong_bson_type;;
let get_timestamp = function | Timestamp v -> v | _ -> raise Wrong_bson_type;;
let get_minkey = function | MinKey -> MinKey | _ -> raise Wrong_bson_type;;
let get_maxkey = function | MaxKey -> MaxKey | _ -> raise Wrong_bson_type;;

(*========================================================================================================*)
(*
  The remove  operations.
*)

let remove_element ename doc = StringMap.remove ename doc;;

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
	    | hd::tl -> trans_doc (i+1) (add_element (String.make 1 (Char.chr (i+48))) hd acc) tl;
	  in
	  let new_doc = trans_doc 0 (make()) v in
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
    (*print_string "e_buff len = "; print_int (Buffer.length e_buf); print_endline "";*)
    Buffer.add_buffer d_buf e_buf;
    Buffer.add_char d_buf '\x00';
    d_buf
  in 
  encode_doc doc;;
    
	  
