<script>
  let { data } = $props();
  let run = $derived(data.run);
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

<a href="/runs">&larr; Back to runs</a>
