```sh
# execute install.sh in test-installer
docker compose run --rm test-installer; echo "Exit Code: $?"

# docker compose run --rm --entrypoint /bin/sh test-installer
```
