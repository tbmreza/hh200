GET http://localhost:9999/add-header
HTTP 404

GET http://localhost:9999/add-header-with-aggregation
header-a: foo
HTTP 404

GET http://localhost:9999/add-header-with-duplicate
header-b: bar
{"message":"hi!"}
HTTP 404
