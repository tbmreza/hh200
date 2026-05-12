# PICKUP laravel upload xls http://localhost:9999/api/xls

POST http://localhost:9999/api/xls
# Content-Type: "application/json"
[MultipartFormData]
# file: "path/to/value"
file: file,examble.xls;
# {
#     "email": "test@example.com",
#     "password": "password"
# }
HTTP 200 204
# [Asserts]
# >jsonpath "$.token" copy
# { "user": { "id": "abc" } }
