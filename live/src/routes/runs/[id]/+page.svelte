<script lang="ts">
  import type { RunDetail } from '$lib/types';
  import StatusBadge from '$lib/components/StatusBadge.svelte';
  import RequestList from '$lib/components/RequestList.svelte';

  let { data } = $props<{ data: { detail: RunDetail } }>();
  const run = $derived(data.detail.run);
</script>

<h1>{run.name}</h1>

<dl>
  <dt>Status</dt>
  <dd><StatusBadge status={run.status} /></dd>
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

<h2>Requests</h2>
<RequestList requests={data.detail.requests} />

<a href="/runs">&larr; Back to runs</a>
