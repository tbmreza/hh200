# live

```
bun run upstream  # served via hh200 is first-class for now
bun run db:fresh
# bun run db:dummy  # truncates app.db with realistic dummy data
```

# sv

To recreate this project with the same configuration when it's started:

```sh
# recreate this project
bun x sv@0.15.4 create --template minimal --no-types --install bun .
```
