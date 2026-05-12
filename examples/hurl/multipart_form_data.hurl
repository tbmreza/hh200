# POST http://localhost:9999/api/xls

POST http://localhost:9999/api/echo
# Content-Type: "application/json"
[MultipartFormData]
file: "path/to/value"
# {
#     "email": "test@example.com",
#     "password": "password"
# }
HTTP 200 204
# [Asserts]
# >jsonpath "$.token" copy
# { "user": { "id": "abc" } }
