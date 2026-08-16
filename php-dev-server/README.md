System-under-test that's under our control.

```bash
composer install
php artisan migrate --seed

cat phpunit.xml
./vendor/bin/phpunit
composer test

php artisan serve --port=9999
```

```
curl -sS -F "description=my excel file" -F "file=@./Area.xlsx" http://localhost:9999/api/register
```

## Summary

- `POST /register` takes `multipart/form-data` with a string `description` and an Excel `file`. The file is stored in the same `xls_files` table as `/api/xls`.


<!-- ```bash -->
<!-- # 1. Success: description + file -->
<!-- printf 'foo,bar\n1,2\n' > /tmp/t.xls -->
<!-- curl -sS -F "description=my excel file" -F "file=@/tmp/t.xls" http://localhost:9999/api/register -->
<!---->
<!-- # 2. Missing description -> 422 -->
<!-- curl -sS -F "file=@/tmp/t.xls" http://localhost:9999/api/register -->
<!---->
<!-- # 3. Missing file -> 422 -->
<!-- curl -sS -F "description=hello" http://localhost:9999/api/register -->
<!---->
<!-- # 4. Wrong field name -> 422 -->
<!-- curl -sS -F "description=hello" -F "wrong=@/tmp/t.xls" http://localhost:9999/api/register -->
<!---->
<!-- # 5. Wrong method (GET) -> 405 -->
<!-- curl -sS http://localhost:9999/api/register -->
<!---->
<!-- # 6. Confirm the upload landed in xls_files -->
<!-- curl -sS http://localhost:9999/api/xls -->
<!-- ``` -->
