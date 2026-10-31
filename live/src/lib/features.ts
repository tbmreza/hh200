// Single source of truth for which backend capabilities the UI can rely on.
// Mirrors the route table in hh200's Dashboard.hs. Keep in sync with the
// backend: flip a flag to `true` only once the endpoint actually exists.

export const features = {
  listRuns: true, // GET /api/runs
  report: true, // GET /api/report/:id
  signal: true, // POST /api/sig
  sse: true, // GET /api/sse
  runDetail: false, // GET /api/runs/:id
  liveRun: false, // GET /api/runs/live
  startRun: false, // (planned) start a run
} as const;

export type Feature = keyof typeof features;
