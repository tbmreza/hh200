<script lang="ts">
	import type { RunDetail } from '$lib/types';
	import { downloadFile } from '$lib/download';
	import { getReportUrl, getReportFilename } from '$lib/api';
	import StatusBadge from '$lib/components/StatusBadge.svelte';
	import RequestList from '$lib/components/RequestList.svelte';
	import MetricWindowList from '$lib/components/MetricWindowList.svelte';

	let { data } = $props<{ data: { detail: RunDetail } }>();
	const run = $derived(data.detail.run);

	async function downloadCsv() {
		await downloadFile(getReportUrl(run.id), getReportFilename(run.id));
	}
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

<button onclick={downloadCsv}>download csv</button>

<h2>Metric windows</h2>
<MetricWindowList metricWindows={data.detail.metric_windows} />

<h2>Requests</h2>
<RequestList requests={data.detail.requests} />

<a href="/runs">&larr; Back to runs</a>
