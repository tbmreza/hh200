POST http://localhost:9999/api/register
Content-Type: "application/json"
[MultipartFormData]
description: "deskrip si"
file: "file,Area.xlsx"
# file: "path/to/value"
# file: file,examble.xls;
# {
#     "email": "test@example.com",
#     "password": "password"
# }
HTTP 200 204
# [Asserts]
# >jsonpath "$.token" copy
# { "user": { "id": "abc" } }

DELETE http://localhost:9999/api/xls/3
HTTP 200
