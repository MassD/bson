Bson.ml
==========

This module includes a Bson document data structure, together with its encoding (to bytes) and decoding (from bytes). 

    The logic of {b usage} is like this
    - Create an empty Bson document
    - Create the elements you want
    - Add elements to the document with names
    - Or remove elements from the document via the names
    - Get elements from the document via the names
    - After obtaining an element, get the raw value from the element

    The functions inside this module seem to be many, however, most of them are just for creating elements. These functions are to {e hide the implementation details of the type elements}. Also, in this way, the Bson document can be used more safely.

    Please refer to the {{: http://bsonspec.org/#/specification } Official Bson specification } for more information.

    {e Version 0.88.0} 