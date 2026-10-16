<script lang="ts">
	import type { Run } from '$lib/types';
	import StatusBadge from './StatusBadge.svelte';

	let { runs }: { runs: Run[] } = $props();
</script>

{#if runs.length === 0}
	<p>No runs yet.</p>
{:else}
	<table>
		<thead>
			<tr>
				<th>Name</th>
				<th>Status</th>
				<th>Script</th>
				<th>Concurrency</th>
				<th>Rate Limit</th>
			</tr>
		</thead>
		<tbody>
			{#each runs as run (run.id)}
				<tr>
					<td><a href="/runs/{run.id}">{run.name}</a></td>
					<td><StatusBadge status={run.status} /></td>
					<td>{run.script_path}</td>
					<td>{run.concurrency}</td>
					<td>{run.rate_limit}</td>
				</tr>
			{/each}
		</tbody>
	</table>
{/if}
