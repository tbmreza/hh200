<script lang="ts">
	import type { Run } from '$lib/types';
	import { downloadFile } from '$lib/download';

	let { data } = $props<{ data: { runs: Run[] } }>();
</script>

<h2>All Runs</h2>
<table>
	<thead>
		<tr>
			<th>Name</th>
			<th>Status</th>
			<th>Script</th>
			<th>Concurrency</th>
			<th>Rate Limit</th>
			<th>Report</th>
		</tr>
	</thead>
	<tbody>
		{#each data.runs as run}
			<tr>
				<td><a href="/runs/{run.name}">{run.name}</a></td>
				<td>{run.status}</td>
				<td>{run.script_path}</td>
				<td>{run.concurrency}</td>
				<td>{run.rate_limit}</td>
				<td>
					<button
						onclick={() => downloadFile(`/api/report/${run.id}`, `stats_history_${run.id}.csv`)}
					>
						download csv
					</button>
				</td>
			</tr>
		{/each}
	</tbody>
</table>
