```
docker compose up -d ocapi-alpine-inst
docker compose logs -f ocapi-alpine-inst

mise exec -- k6 run hello.js
mise exec -- k6 run --vus 10 --duration 30s hello.js
mise exec -- k6 run open-model.js

```
