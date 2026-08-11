<script>
  let { data } = $props();
  let run = $derived(data.run);

  async function downloadCsv() {
    if (!run) return;
    const res = await fetch(`/api/report/${run.id}`);
    if (!res.ok) return;
    const blob = await res.blob();
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `stats_history_${run.id}.csv`;
    a.click();
    URL.revokeObjectURL(url);
  }
</script>

<h1>{run.name}</h1>

<dl>
  <dt>Status</dt>
  <dd>{run.status}</dd>
  <dt>Script path</dt>
  <dd>{run.script_path}</dd>
  <dt>Started at</dt>
  <dd>{new Date(run.started_at * 1000).toLocaleString()}</dd>
  <dt>Ended at</dt>
  <dd>{run.ended_at ? new Date(run.ended_at * 1000).toLocaleString() : 'N/A'}</dd>
  <dt>Concurrency</dt>
  <dd>{run.concurrency}</dd>
  <dt>Rate limit</dt>
  <dd>{run.rate_limit}</dd>
  <dt>Control socket</dt>
  <dd>{run.control_socket}</dd>
</dl>

<button onclick={downloadCsv}>download csv</button>

<a href="/runs">&larr; Back to runs</a>
