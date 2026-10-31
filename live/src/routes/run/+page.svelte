<script lang="ts">
  import { features } from '$lib/features';
  import { previewRun, type RunConfig, type Foreseeable } from '$lib/preview';

  let config = $state<RunConfig>({
    name: '',
    script: '',
    nvu: 1,
    duration: 0,
  });

  const preview = $derived(previewRun(config));

  function describe<T>(f: Foreseeable<T>, format: (v: T) => string): string {
    return f.known ? format(f.value) : `unknown — ${f.reason}`;
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
    <input type="number" min="0" bind:value={config.nvu} />
  </label>

  <label>
    Duration (seconds, 0 = single-shot)
    <input type="number" min="0" bind:value={config.duration} />
  </label>
</form>

<h2>What we can foresee</h2>
<dl>
  <dt>In-flight ceiling</dt>
  <dd>{preview.inFlightCeiling}</dd>

  <dt>Wall clock</dt>
  <dd>{preview.wallClock}</dd>

  <dt>Total requests</dt>
  <dd>{describe(preview.totalRequests, (v) => `~${v}`)}</dd>

  <dt>Rate limit</dt>
  <dd>{describe(preview.rateLimit, (v) => `${v}/s`)}</dd>
</dl>

<button type="button" disabled={!features.startRun} title="The backend has no start-run API yet">
  Run
</button>

{#if !features.startRun}
  <p class="note">Run is not implemented yet — the backend has no start-run API.</p>
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
</style>
