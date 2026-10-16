<script lang="ts">
	import type { Request } from '$lib/types';

	let { requests }: { requests: Request[] } = $props();

	let expanded = $state<Record<number, boolean>>({});

	function toggle(id: number) {
		expanded[id] = !expanded[id];
	}
</script>

{#if requests.length === 0}
	<p>No requests recorded.</p>
{:else}
	<table>
		<thead>
			<tr>
				<th>Seq</th>
				<th>Method</th>
				<th>URL</th>
				<th>Status</th>
				<th>Duration (ms)</th>
				<th>Bytes in</th>
				<th>Bytes out</th>
				<th>Worker</th>
				<th></th>
			</tr>
		</thead>
		<tbody>
			{#each requests as request (request.id)}
				<tr>
					<td>{request.seq}</td>
					<td>{request.method}</td>
					<td>{request.url}</td>
					<td>{request.status_code ?? '—'}</td>
					<td>{request.duration_ms ?? '—'}</td>
					<td>{request.bytes_in ?? '—'}</td>
					<td>{request.bytes_out ?? '—'}</td>
					<td>{request.worker_id ?? '—'}</td>
					<td>
						<button onclick={() => toggle(request.id)}>
							{expanded[request.id] ? 'hide' : 'show'}
						</button>
					</td>
				</tr>
				{#if expanded[request.id]}
					<tr>
						<td colspan="9">
							{#if request.error}
								<p class="error">error: {request.error}</p>
							{/if}
							{#each request.headers as header (header.id)}
								<div class="kv">
									<span class="dir">{header.direction}</span>
									<strong>{header.name}:</strong> {header.value}
								</div>
							{/each}
							{#each request.bodies as body (body.id)}
								<div class="body">
									<span class="dir">{body.direction} body</span>
									{#if body.truncated}<em>(truncated)</em>{/if}
									<pre>{body.content ?? ''}</pre>
								</div>
							{/each}
						</td>
					</tr>
				{/if}
			{/each}
		</tbody>
	</table>
{/if}

<style>
	.error {
		color: #b91c1c;
	}
	.dir {
		display: inline-block;
		margin-right: 0.5rem;
		text-transform: uppercase;
		font-size: 0.8em;
		color: #6b7280;
	}
	.kv {
		font-family: monospace;
	}
	pre {
		white-space: pre-wrap;
		word-break: break-all;
	}
</style>
