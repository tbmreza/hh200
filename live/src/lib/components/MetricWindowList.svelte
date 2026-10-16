<script lang="ts">
	import type { MetricWindow } from '$lib/types';

	let { metricWindows }: { metricWindows: MetricWindow[] } = $props();
</script>

{#if metricWindows.length === 0}
	<p>No metric windows recorded.</p>
{:else}
	<table>
		<thead>
			<tr>
				<th>Endpoint</th>
				<th>Start</th>
				<th>End</th>
				<th>Count</th>
				<th>Fails</th>
				<th>Min (ms)</th>
				<th>Avg (ms)</th>
				<th>Max (ms)</th>
				<th>RPS</th>
			</tr>
		</thead>
		<tbody>
			{#each metricWindows as mw (mw.id)}
				<tr>
					<td>{mw.endpoint}</td>
					<td>{new Date(mw.start).toLocaleString()}</td>
					<td>{new Date(mw.end).toLocaleString()}</td>
					<td>{mw.count}</td>
					<td>{mw.fail_count}</td>
					<td>{mw.min_latency}</td>
					<td>{mw.avg_latency}</td>
					<td>{mw.max_latency}</td>
					<td>{mw.rps}</td>
				</tr>
			{/each}
		</tbody>
	</table>
{/if}
