# live

> After a fresh clone/checkout, run `bun run schema` once to regenerate the
> Prisma client (`src/generated/prisma` is gitignored and not committed).

```
nvm use
bun run upstream
bun run dev
```

## Developing schema

```
bun run schema  # also updates ../../hh200-docs/docs/contributing/database-schema.md
bun run apply
```
