<script lang="ts">
	import type { Run } from '$lib/types';
	import StatusBadge from './StatusBadge.svelte';

	let { runs }: { runs: Run[] } = $props();
</script>

{#if runs.length === 0}
	<div class="empty-state">
		<span class="empty-icon">◌</span>
		<strong>No runs yet</strong>
		<p>Start a load test to see its activity and results here.</p>
		<a href="/run">Create your first run <span>→</span></a>
	</div>
{:else}
	<div class="table-wrap">
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
						<td><a class="run-link" href="/runs/{run.id}">{run.name}<span>→</span></a></td>
						<td><StatusBadge status={run.status} /></td>
						<td class="muted">{run.script_path}</td>
						<td>{run.concurrency}</td>
						<td>{run.rate_limit}<span class="unit">/s</span></td>
					</tr>
				{/each}
			</tbody>
		</table>
	</div>
{/if}

<style>
	.table-wrap {
		overflow-x: auto;
		border: 1px solid #e6eaf1;
		border-radius: 1rem;
		background: white;
		box-shadow: 0 10px 28px #17203308;
	}

	table {
		width: 100%;
		border-collapse: collapse;
		min-width: 700px;
	}

	th,
	td {
		padding: 1rem 1.15rem;
		border-bottom: 1px solid #eef1f5;
		text-align: left;
		white-space: nowrap;
	}

	th {
		background: #f8fafc;
		color: #64748b;
		font-size: 0.68rem;
		font-weight: 800;
		letter-spacing: 0.08em;
		text-transform: uppercase;
	}

	td {
		color: #334155;
		font-size: 0.84rem;
	}

	tbody tr:last-child td {
		border-bottom: 0;
	}

	tbody tr:hover {
		background: #fafaff;
	}

	.run-link {
		display: inline-flex;
		align-items: center;
		gap: 0.5rem;
		color: #4f46e5;
		font-weight: 700;
		text-decoration: none;
	}

	.run-link span {
		opacity: 0;
		transition: opacity 150ms ease, transform 150ms ease;
	}

	.run-link:hover span {
		opacity: 1;
		transform: translateX(2px);
	}

	.muted {
		color: #64748b;
	}

	.unit {
		margin-left: 0.2rem;
		color: #94a3b8;
		font-size: 0.75rem;
	}

	.empty-state {
		display: grid;
		justify-items: center;
		padding: 4rem 1rem;
		border: 1px dashed #cbd5e1;
		border-radius: 1rem;
		background: white;
		text-align: center;
	}

	.empty-icon {
		display: grid;
		width: 3rem;
		height: 3rem;
		place-items: center;
		margin-bottom: 1rem;
		border-radius: 1rem;
		background: #eef2ff;
		color: #6366f1;
		font-size: 1.75rem;
	}

	.empty-state strong {
		color: #1e293b;
	}

	.empty-state p {
		margin: 0.4rem 0 1.1rem;
		color: #64748b;
		font-size: 0.82rem;
	}

	.empty-state a {
		color: #4f46e5;
		font-size: 0.82rem;
		font-weight: 700;
		text-decoration: none;
	}

	.empty-state a span {
		margin-left: 0.25rem;
	}
</style>
