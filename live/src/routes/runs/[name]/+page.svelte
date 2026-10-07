<script lang="ts">
	import type { Run } from '$lib/types';
	import { downloadFile } from '$lib/download';

	let { data } = $props<{ data: { run: Run } }>();
	let run = $derived(data.run);

	async function downloadCsv() {
		await downloadFile(`/api/report/${run.id}`, `stats_history_${run.id}.csv`);
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
