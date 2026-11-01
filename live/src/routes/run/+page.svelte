<script lang="ts">
  import { goto } from '$app/navigation';
  import { features } from '$lib/features';
  import { previewRun, type RunConfig } from '$lib/preview';
  import { planRun, startRun } from '$lib/api';
  import type { RunPlan, Foreseeable } from '$lib/types';

  let config = $state<RunConfig>({
    name: '',
    script: '',
    nvu: 1,
    duration: 0,
  });

  let plan = $state<RunPlan | null>(null);
  let starting = $state(false);
  let error = $state<string | null>(null);

  const preview = $derived(previewRun(config));

  // Re-fetch the server's pre-flight whenever the inputs change, dropping
  // stale responses via the effect teardown.
  $effect(() => {
    const current = { ...config };
    let cancelled = false;
    planRun(current)
      .then((p) => {
        if (!cancelled) plan = p;
      })
      .catch(() => {
        if (!cancelled) plan = null;
      });
    return () => {
      cancelled = true;
    };
  });

  function describe<T>(f: Foreseeable<T>, format: (v: T) => string): string {
    return f.known ? format(f.value) : `unknown — ${f.reason}`;
  }

  async function submit() {
    starting = true;
    error = null;
    try {
      const { id } = await startRun(config);
      await goto(`/runs/${id}`);
    } catch (e) {
      error = e instanceof Error ? e.message : String(e);
    } finally {
      starting = false;
    }
  }
</script>

<h1>Run</h1>

<form>
  <label>
    Name
    <input type="text" bind:value={config.name} placeholder="my-run" />
  </label>

  <label>
    Script path (.hhs)
    <input type="text" bind:value={config.script} placeholder="/scripts/example.hhs" />
  </label>

  <label>
    Virtual users (nvu)
    <input type="number" min="1" bind:value={config.nvu} />
  </label>

  <label>
    Duration (seconds, 0 = single-shot)
    <input type="number" min="0" bind:value={config.duration} />
  </label>
</form>

{#if plan?.errors?.length}
  <ul class="errors">
    {#each plan.errors as err (err.line + ':' + err.column)}
      <li>line {err.line}, column {err.column}: {err.message}</li>
    {/each}
  </ul>
{/if}

<h2>What we can foresee</h2>
<dl>
  <dt>In-flight ceiling</dt>
  <dd>{plan?.inFlightCeiling ?? preview.inFlightCeiling}</dd>

  <dt>Wall clock</dt>
  <dd>{plan?.wallClock ?? preview.wallClock}</dd>

  <dt>Total requests</dt>
  <dd>{describe(plan?.totalRequests ?? preview.totalRequests, (v) => `~${v}`)}</dd>

  <dt>Rate limit</dt>
  <dd>{describe(plan?.rateLimit ?? preview.rateLimit, (v) => `${v}/s`)}</dd>
</dl>

<button
  type="button"
  disabled={starting || plan?.scriptValid === false}
  onclick={submit}
>
  {starting ? 'Starting…' : 'Run'}
</button>

{#if !features.startRun}
  <p class="note">Run is not implemented yet — the backend has no start-run API.</p>
{/if}

{#if error}
  <p class="error">{error}</p>
{/if}

<style>
  form {
    display: grid;
    grid-template-columns: max-content 1fr;
    gap: 0.5rem 1rem;
    max-width: 32rem;
  }
  label {
    display: contents;
  }
  input {
    padding: 0.25rem 0.5rem;
  }
  dl {
    display: grid;
    grid-template-columns: max-content 1fr;
    gap: 0.25rem 1rem;
    max-width: 32rem;
  }
  dt {
    color: #6b7280;
  }
  dd {
    margin: 0;
  }
  button {
    margin-top: 1rem;
    padding: 0.5rem 1.5rem;
  }
  .note {
    color: #6b7280;
    font-size: 0.9em;
  }
  .error {
    color: #b91c1c;
  }
  .errors {
    color: #b91c1c;
    margin: 0.5rem 0;
    padding-left: 1.25rem;
  }
</style>
