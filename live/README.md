# live

SvelteKit frontend for the hh200 dashboard. It is built to static files and
served by the Haskell backend in `../hh200`.

## Quick start

```
nvm use          # pin Node to the version in .nvmrc (v24)
bun install      # install dependencies
bun run schema   # generate the Prisma client (src/generated/prisma is gitignored)
bun run upstream # build the site into ../hh200/min (what the backend serves)
bun run dev      # start the Vite dev server for frontend work
```

> `bun run schema` also writes `../../hh200-docs/docs/contributing/database-schema.md`,
> so that sibling repo must be checked out alongside this one.

## Database

The Prisma schema (`prisma/schema.prisma`) targets `prisma/app.db` (gitignored).
The Haskell dashboard reads a *different* database by default (`HH200_SQLITE`, or
`~/.local/share/hh200/hh200.sqlite`). Point the dashboard at this project's
database to see the data you create here:

```
HH200_SQLITE=$PWD/prisma/app.db hh200 browse
```

Create the schema and seed dummy data:

```
bun run apply   # apply pending migrations to prisma/app.db
bun run dummy   # insert fake runs / requests / metric windows
```

## Developing schema

Edit `prisma/schema.prisma`, then:

```
bun run schema  # regenerate the client and database-schema.md
bun run apply   # create & apply a migration (prisma migrate dev)
```

`bun run prisma:nuke` deletes the generated client, migrations, and `app.db` to
start from scratch.

## Backend/frontend schema drift

The two halves are still converging, so only part of the Prisma schema is
consumed by the backend today:

- `runs` — fully aligned (served by `GET /api/runs`).
- `metric_windows` — the backend reads only `id`, `run_id`, and `rps`
  (for `GET /api/report/:id`).
- `requests`, `request_headers`, `request_bodies` — defined and seeded here but
  not yet exposed by any backend route (`GET /api/runs/:id` and
  `GET /api/runs/live` are still unimplemented).

## Principles for review

- **Preview before fire.** Show a configuration's effective (resolved) values
  and foreseeable consequences before any action. Mirroring the CLI's
  `--debug-config`, the Run screen states what will happen (in-flight ceiling,
  rate cap, request estimates, duration) before the Run button is actionable.
- **A lens on the CLI, not a replacement.** The `.hhs` script and CLI flags stay
  the source of truth; the UI previews and observes, it doesn't invent config it
  can't honour.
- **Static form factor.** Static SvelteKit build served by the
  backend; all dynamic data flows through the existing HTTP + SSE surface.
- **Metrics and monitoring are first-class.** A dashboard showing latency, rps,
  and failures per window is a goal, not an afterthought. We build the surface
  now — even where the backend's metric data is still thin — because a visible
  surface creates pressure for real data.
<!-- - Not yet: auth, multi-tenancy, alerting — those come after the core observe-and-preview loop. -->
<!-- - **Honesty about gaps.** Anything not implemented is rendered as not -->
<!--   implemented — disabled, labelled, with a reason — never as a silently-broken -->
<!--   control or a blank page. `src/lib/features.ts` is the single source of truth -->
<!--   for what's available. -->
