# use the dedicated [MultipartFormData] section
POST http://localhost:9999/multipart-form-data
# [MultipartFormData]
# key1: value1
# upload1: file,data.txt;
# upload2: file,data.html;
# upload3: file,data.txt; text/html
HTTP 200
