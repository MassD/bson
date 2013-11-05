let content_id = "52780d55c21477f7aa5b9108" 
in
let objectId = Bson.create_objectId content_id in
let bson_condition = Bson.add_element "_id" objectId Bson.empty in
print_endline (Bson.to_simple_json bson_condition)
