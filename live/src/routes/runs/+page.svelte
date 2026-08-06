<script>
  import { onMount } from 'svelte';
  let { data } = $props();

  // ??: if live dashboard means an event represents a new row or a row field update
  onMount(() => {
    const es = new EventSource('/sse');
    es.onmessage = (e) => console.log('event received', e.data);
    return () => es.close();
  });
</script>

<h1>Runs</h1>

{#each data.runs as run}
  <article>
    <h2><a href="/runs/{run.name}">{run.name}</a></h2>
    <p>Status: {run.status}</p>
    <p>Script: {run.script_path}</p>
    <p>Concurrency: {run.concurrency}</p>
    <p>Rate limit: {run.rate_limit}</p>
  </article>
{/each}
