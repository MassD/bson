module type BSONSig =
sig

  type t;;
  type empty = 
    | NULL 
    | MINKEY
    | MAXKEY;;
  type element;;
  
    
  exception Invalid_objectId;;
  exception Wrong_bson_type;;
  exception Wrong_string;;
  exception Malformed_doc;;

  val make : unit -> t;;

  val add_element : string -> element -> t -> t;;

  val create_double : float -> element;;
  val create_string : string -> element;;
  val create_doc_element : t -> element;;
  val create_list : element list -> element;;
  
  val create_generic_binary : string -> element;;
  val create_function_binary : string -> element;;
  val create_uuid_binary : string -> element;;
  val create_md5_binary : string -> element;;
  val create_user_binary : string -> element;;
  
  val create_objectId : string -> element;;
  val create_boolean : bool -> element;;
  val create_utc : int64 -> element;;
  val create_null : unit -> element;;
  val create_regex : string -> string -> element;;
  val create_jscode : string -> element;;
  val create_jscode_w_s : string -> t -> element;;
  val create_int32 : int32 -> element;;
  val create_int64 : int64 -> element;;
  val create_timestamp : int64 -> element;;
  val create_minkey : unit -> element;;
  val create_maxkey : unit -> element;;
   
  val get_element : string -> t -> element;;
  
  val get_double : element -> float;;
  val get_string : element -> string;;
  val get_doc_element : element -> t;;
  val get_list : element -> element list;;
  
  val get_generic_binary : element -> string;;
  val get_function_binary : element -> string;;
  val get_uuid_binary : element -> string;;
  val get_md5_binary : element -> string;;
  val get_user_binary : element -> string;;

  val get_objectId : element -> string;;
  val get_boolean : element -> bool;;
  val get_utc : element -> int64;;
  val get_null : element -> empty;;
  val get_regex : element -> (string * string);;
  val get_jscode : element -> string;;
  val get_jscode_w_s : element -> (string * t);;
  val get_int32 : element -> int32;;
  val get_int64 : element -> int64;;
  val get_timestamp : element -> int64;;
  val get_minkey : element -> empty;;
  val get_maxkey : element -> empty;;

  val remove_element : string -> t -> t;;

  val encode : t -> string;;
end;;

module BSON : BSONSig =
struct
  (* 
   Define the data structure of bson in ocaml.
   bson in ocaml is actually a Map whose key is string and value is element.
  *)
  module StringMap = Map.Make(struct type t = string let compare = compare end);;

  type document = element StringMap.t
  and t = document
  and empty = 
    | NULL
    | MINKEY
    | MAXKEY
  and element = 
    | Double of float
    | String of string
    | Document of document
    | Array of element list
    | Binary of binary
    | ObjectId of string (* only 12 bytes *)
    | Boolean of bool
    | UTC of int64
    | Null of empty
    | Regex of (string * string)
    | JSCode of string
    | JSCodeWS of (string * document)
    | Int32 of int32
    | Int64 of int64
    | Timestamp of int64
    | MinKey of empty
    | MaxKey of empty
  and binary =
    | Generic of string
    | Function of string
    | UUID of string
    | MD5 of string
    | UserDefined of string;;

  (* create an emapty bson doc *)
  let make () = StringMap.empty;;
  
  (*
    for constructing a document
    1. we make a empty document
    2. we create element as we want
    3. we add the element to the document, with a element name
  *)
  let add_element ename element doc = StringMap.add ename element doc;;

  let create_double v = Double v;;
  let create_string v = String v;;
  let create_doc_element v = Document v;;
  let create_list l = Array l;;

  let create_generic_binary v = Binary (Generic v);;
  let create_function_binary v = Binary (Function v);;
  let create_uuid_binary v = Binary (UUID v);;
  let create_md5_binary v = Binary (MD5 v);;
  let create_user_binary v = Binary (UserDefined v);;

  exception Invalid_objectId;;
  let is_valid_objectId objectId = if String.length objectId = 12 then true else false;;
  let create_objectId v = 
    if is_valid_objectId v then ObjectId v
    else raise Invalid_objectId;;

  let create_boolean v = Boolean v;;
  let create_utc v = UTC v;;
  let create_null () = Null NULL;;
  let create_regex s1 s2 = Regex (s1, s2);;
  let create_jscode v = JSCode v;;
  let create_jscode_w_s s doc = JSCodeWS (s, doc);;
  let create_int32 v = Int32 v;;
  let create_int64 v = Int64 v;;
  let create_timestamp v = Timestamp v;;
  let create_minkey () = MinKey MINKEY;;
  let create_maxkey () = MaxKey MAXKEY;;

  (*
  for using a document
  1. we get an element from document, if existing
  2. we get the value of the element
  *)
  exception Wrong_bson_type;;

  let get_element ename doc = StringMap.find ename doc;;

  let get_double = function | Double v -> v | _ -> raise Wrong_bson_type;;
  let get_string = function | String v -> v | _ -> raise Wrong_bson_type;;
  let get_doc_element = function | Document v -> v | _ -> raise Wrong_bson_type;;
  let get_list = function | Array v -> v | _ -> raise Wrong_bson_type;;
  let get_generic_binary = function | Binary (Generic v) -> v | _ -> raise Wrong_bson_type;;
  let get_function_binary = function | Binary (Function v) -> v | _ -> raise Wrong_bson_type;;
  let get_uuid_binary = function | Binary (UUID v) -> v | _ -> raise Wrong_bson_type;;
  let get_md5_binary = function | Binary (MD5 v) -> v | _ -> raise Wrong_bson_type;;
  let get_user_binary = function | Binary (UserDefined v) -> v | _ -> raise Wrong_bson_type;;
  let get_objectId = function | ObjectId v -> v | _ -> raise Wrong_bson_type;;
  let get_boolean = function | Boolean v -> v | _ -> raise Wrong_bson_type;;
  let get_utc = function | UTC v -> v | _ -> raise Wrong_bson_type;;
  let get_null = function | Null NULL -> NULL | _ -> raise Wrong_bson_type;;
  let get_regex = function | Regex v -> v | _ -> raise Wrong_bson_type;;
  let get_jscode = function | JSCode v -> v | _ -> raise Wrong_bson_type;;
  let get_jscode_w_s = function | JSCodeWS v -> v | _ -> raise Wrong_bson_type;;
  let get_int32 = function | Int32 v -> v | _ -> raise Wrong_bson_type;;
  let get_int64 = function | Int64 v -> v | _ -> raise Wrong_bson_type;;
  let get_timestamp = function | Timestamp v -> v | _ -> raise Wrong_bson_type;;
  let get_minkey = function | MinKey MINKEY -> MINKEY | _ -> raise Wrong_bson_type;;
  let get_maxkey = function | MaxKey MAXKEY -> MAXKEY | _ -> raise Wrong_bson_type;;

  (*
  The remove  operations.
  *)

  let remove_element ename doc = StringMap.remove ename doc;;

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

  let encode_int64 buf v =
    for i = 0 to 7 do
      let b = Int64.logand 255L (Int64.shift_right v (i*8)) in
      Buffer.add_char buf (Char.chr (Int64.to_int b))
    done;;

  let encode_float buf v = encode_int64 buf (Int64.bits_of_float v);;

  let encode_int32 buf v = 
    for i = 0 to 3 do
      let b = Int32.logand 255l (Int32.shift_right v (i*8)) in
      Buffer.add_char buf (Char.chr (Int32.to_int b))
    done;;
  
  let encode_ename buf c ename = 
    Buffer.add_char buf c; 
    Buffer.add_string buf ename; 
    Buffer.add_char buf '\x00';;

  exception Wrong_string;;

  let encode_string buf s =
    let len = String.length s in 
    if s.[len-1] = '\x00' then raise Wrong_string
    else begin
      encode_int32 buf (Int32.of_int ((String.length s) + 1));
      Buffer.add_string buf s;
      Buffer.add_char buf '\x00'
    end;;

  let encode_objectId buf s = 
    if String.length s <> 12 then raise Invalid_objectId
    else encode_string buf s;;
    
  let encode_binary buf c b = 
    encode_int32 buf (Int32.of_int (String.length b));
    Buffer.add_char buf c;
    Buffer.add_string buf b;;

  let encode_cstring buf cs = 
    Buffer.add_string buf cs; 
    Buffer.add_char buf '\x00';;
  
  let list_to_doc l = (* we need to transform the list to a doc with key as incrementing from '0' *)
    let rec to_doc i acc = function 
      | [] -> acc
      | hd::tl -> to_doc (i+1) (add_element (String.make 1 (Char.chr (i+48))) hd acc) tl
    in
    to_doc 0 (make()) l;;
    
  exception Malformed_doc;;
  let encode doc =
    let all_buf = Buffer.create 64 in
    let rec encode_element buf ename element = 
      match element with
	| Double v -> 
	  encode_ename buf '\x01' ename;
	  encode_float buf v
	| String v -> 
	  encode_ename buf '\x02' ename;
	  encode_string buf v
	| Document v -> 
	  encode_ename buf '\x03' ename;
	  encode_doc buf v
	| Array v ->
	  encode_ename buf '\x04' ename;
	  encode_doc buf (list_to_doc v)
	| Binary v ->
	  encode_ename buf '\x05' ename;
	  begin match v with
	    | Generic v -> encode_binary buf '\x00' v
	    | Function v -> encode_binary buf '\x01' v
	    | UUID v -> encode_binary buf '\x04' v
	    | MD5 v -> encode_binary buf '\x05' v
	    | UserDefined v -> encode_binary buf '\x80' v
	  end 
	| ObjectId v -> 
	  encode_ename buf '\x07' ename;
	  encode_objectId buf v
	| Boolean v ->
	  encode_ename buf '\x08' ename;
	  Buffer.add_char buf (if v then '\x00' else '\x01')
	| UTC v ->
	  encode_ename buf '\x09' ename;
	  encode_int64 buf v
	| Null NULL->
	  encode_ename buf '\x0A' ename;
	| Regex (v1,v2) ->
	  encode_ename buf '\x0B' ename;
	  encode_cstring buf v1;
	  encode_cstring buf v2
	| JSCode v ->
	  encode_ename buf '\x0D' ename;
	  encode_string buf v
	| JSCodeWS (v, d) ->
	  encode_ename buf '\x0F' ename;
	  let tmp_str_buf = Buffer.create 16 and tmp_doc_buf = Buffer.create 16 in
	  encode_string tmp_str_buf v;
	  encode_doc tmp_doc_buf d;
	  encode_int32 buf (Int32.of_int ((Buffer.length tmp_str_buf) + (Buffer.length tmp_doc_buf)));
	  Buffer.add_buffer buf tmp_str_buf;
	  Buffer.add_buffer buf tmp_doc_buf
	| Int32 v -> 
	  encode_ename buf '\x10' ename;
	  encode_int32 buf v
	| Timestamp v -> 
	  encode_ename buf '\x11' ename;
	  encode_int64 buf v
	| Int64 v -> 
	  encode_ename buf '\x12' ename;
	  encode_int64 buf v
	| MinKey MINKEY ->
	  encode_ename buf '\xFF' ename
	| MaxKey MAXKEY ->
	  encode_ename buf '\x7F' ename
	| _ -> raise Malformed_doc
    and 
    encode_doc buf doc = 
      let bindings = StringMap.bindings doc in
      let process_element buf (ename, element) = encode_element buf ename element; buf in
      let e_buf = List.fold_left process_element (Buffer.create 64) bindings in
      encode_int32 buf (Int32.of_int (5+(Buffer.length e_buf)));
      Buffer.add_buffer buf e_buf;
      Buffer.add_char buf '\x00';
    in 
    encode_doc all_buf doc;
    Buffer.contents all_buf;;

end;;
