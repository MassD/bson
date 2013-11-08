#Bson.ml

**Bson.ml** is an ocaml module that includes a Bson document data structure, together with its encoding (to bytes) and decoding (from bytes). 

###The logic of **usage** is like this

1. Create an empty Bson document
2. Create the elements you want
3. Add elements to the document with names
4. Or remove elements from the document via the names
5. Get elements from the document via the names
6. After obtaining an element, get the raw value from the element

The functions inside this module seem to be many, however, most of them are just for creating elements. These functions are to *hide the implementation details of the type elements*. Also, in this way, the Bson document can be used more safely.

###Sample code

    let doc = Bson.empty;;
    let element = Bson.create_string "world";;
	let new_doc = Bson.add_element "hello" element doc;;
    
    let encoded_bytes = Bson.encode new_doc;;
    let decoded_doc = Bson.decode encoded_bytes;;

###Prequistie

Bson.ml does not use any external / 3rd party libraries. 

However, `ocamlbuild` is recommended for the building job, though it is included in ocaml.

###Taste

In order to taste Bson.ml, please have a look at test/test_bson.ml. 
You can use the following command in the root of the project to run it:

	ocamlbuild -I src test/test_bson.native
	./test_bson.native 

###Install
##### OPAM

	opam install bson

##### From source

	make
	make install


###Bson.syntax

Deriving syntax extension.

	type t = {
	  name = string;
	  value = int;
	} deriving (Bson_ext)

generate Bson_utils_t.to_bson and Bson_utils_t.from_bson

#####Example

	let user = {
	  name = "Joe"
	  value = 5;
	} deriving (Bson_ext);
	
	Mongo_lwt.insert mongo [ Bson_utils_t.to_bson user ];
	(* ..... *)
	let ds = MongoReply.get_document_list r in
	List.fold_left (
         fun acc d ->
          (Bson_utils_t.from_bson d)::acc
         ) acc ds

###Misc
Please also refer to the [Official Bson specification](http://bsonspec.org/#/specification) for more information.

*Version 0.89.2* 

*Yes, I would like to call this utility as Bson.ml instead of ocamlbson, or something like that.* 

*In addition, experienced ocaml developers are welcomed to improve the code base*

