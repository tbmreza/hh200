# live

```
bun run build
bun run upstream  # served via hh200 is first-class for now
```

# sv

To recreate this project with the same configuration when it's started:

```sh
# recreate this project
bun x sv@0.15.4 create --template minimal --no-types --install bun .
```
<button onclick={async () => await fetch('/api/runs')}>stop2</button>
