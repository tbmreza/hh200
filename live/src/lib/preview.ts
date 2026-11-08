// Pure, client-side "preview before commit" for the Run form.
//
// Mirrors what the CLI can foresee from its flags (`--nvu`, `--duration`) and
// flags what it cannot until a `--debug-config`-style backend endpoint exists
// (e.g. the rate limit, which is resolved from the script header/environment).
// The authoritative pre-flight now comes from `POST /api/run/plan`; this
// module is the instant, optimistic fallback.

import type { Foreseeable, RunRequest } from './types';
import { rampCurve, type ConcurrencyPoint } from './ramp';

export type RunConfig = RunRequest;

export type RunPreview = {
  inFlightCeiling: number; // == nvu, always foreseeable
  wallClock: string; // 'single-shot' or '<n> seconds'
  rateLimit: Foreseeable<number>; // unknown until script/env is resolved
  totalRequests: Foreseeable<number>; // ~nvu for single-shot, else unknown
  rampCurve: ConcurrencyPoint[]; // planned active VUs over time
};

export function previewRun(config: RunConfig): RunPreview {
  const nvu = Number.isFinite(config.nvu) ? Math.max(0, Math.floor(config.nvu)) : 0;
  const durationSec = Number.isFinite(config.duration)
    ? Math.max(0, Math.floor(config.duration))
    : 0;
  const rampUpSec = Number.isFinite(config.rampUp) ? Math.max(0, config.rampUp) : 0;
  const singleShot = durationSec === 0;

  const rateLimit: Foreseeable<number> = {
    known: false,
    reason:
      'resolved from the script header/environment (--debug-config), not yet available in the UI',
  };

  const totalRequests: Foreseeable<number> = singleShot
    ? { known: true, value: nvu }
    : { known: false, reason: 'depends on the resolved rate limit' };

  return {
    inFlightCeiling: nvu,
    wallClock: singleShot ? 'single-shot' : `${durationSec} seconds`,
    rateLimit,
    totalRequests,
    rampCurve: rampCurve(nvu, rampUpSec, durationSec),
  };
}
