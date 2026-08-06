System-under-test that's under our control.

```bash
composer install
php artisan migrate --seed

cat phpunit.xml
./vendor/bin/phpunit
composer test

php artisan serve --port=9999
```
