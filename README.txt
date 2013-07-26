(* OASIS_START *)
(* DO NOT EDIT (digest: 6fe1904f991c5700dec4a476c81dba70) *)
This is the README file for the bson.ml distribution.

A bson data structure, including encoding/decoding

See the files INSTALL.txt for building and installation instructions. 


(* OASIS_STOP *)

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

###Ocamlfind support

The META file is supplied in the root of project. Also the _build folder is provided together with the source.

If you want to directly use it as a library via ocamlfind, please do

	ocamlfind install bson META _build/src/bson.cmx _build/src/bson.cmo _build/src/bson.cmi _build/src/bson.o


###Misc
Please also refer to the [Official Bson specification](http://bsonspec.org/#/specification) for more information.

*Version 0.88.5* 

*Yes, I would like to call this utility as Bson.ml instead of ocamlbson, or something like that.* 

*In addition, experienced ocaml developers are welcomed to improve the code base*

