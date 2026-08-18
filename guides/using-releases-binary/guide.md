```sh
# docker compose run --rm <service name>; echo "Exit Code: $?"  # basic test
# docker compose run --rm -it <service name> /bin/sh            # interactive debug shell

docker compose -f glibc-docker-compose.yml up --abort-on-container-exit --exit-code-from hh200-debian-instance
```
