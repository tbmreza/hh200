```
docker compose up -d --force-recreate ocapi-alpine-inst
docker compose logs -f ocapi-alpine-inst                       # docker container log
docker compose exec ocapi-alpine-inst tail -f /logs/ocapi.log  # ocapi traffic log

mise exec -- k6 run hello.js
mise exec -- k6 run --vus 10 --duration 30s hello.js
mise exec -- k6 run open-model.js
```

## Logs

- **Traffic dump** (`logs/ocapi.log`, bind-mounted into the container at
  `/logs/ocapi.log`): arrival/completion timing records (`A <id> <ns>` /
  `C <id> <ns>`), rotated by host logrotate via `copytruncate` (safe because
  ocapi opens the file with `O_APPEND`).

  Install the rotation config once:

  ```
  sudo ln -s "$PWD/logrotate.d/ocapi" /etc/logrotate.d/ocapi
  ```

  Then follow it from the host with `tail -f logs/ocapi.log` or inside the
  container with `docker compose exec ocapi-alpine-inst tail -f /logs/ocapi.log`.

- **Container logs** (`docker compose logs`): `tracing` output on stdout with
  `method`/`uri`/`status` per request, for debugging. Not rotated; size-capped
  by the Docker json-file driver (`max-size`/`max-file` in `docker-compose.yml`).
