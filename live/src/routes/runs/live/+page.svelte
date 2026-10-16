<script lang="ts">
	import { onMount } from 'svelte';
	import type { LiveRun, RunControlState, RunSignal } from '$lib/types';
	import { live, setLiveRun, connectSse } from '$lib/sse';
	import type { LiveState } from '$lib/sse';
	import { sendSignal } from '$lib/api';
	import StatusBadge from '$lib/components/StatusBadge.svelte';
	import ControlBar from '$lib/components/ControlBar.svelte';

	let { data } = $props<{ data: { live: LiveRun } }>();

	let current = $state<LiveState>({ run: null, state: 'running' });

	$effect(() => {
		return live.subscribe((v) => (current = v));
	});

	onMount(() => {
		setLiveRun(data.live.run, data.live.state);
		return connectSse();
	});

	async function handleSignal(signal: RunSignal) {
		const next: Record<RunSignal, RunControlState> = {
			pause: 'paused',
			resume: 'running',
			stop: 'stopped',
		};
		current = { ...current, state: next[signal] };
		await sendSignal(signal);
	}
</script>

<h1>Live Run</h1>

{#if current.run}
	<article>
		<h2><a href="/runs/{current.run.id}">{current.run.name}</a></h2>
		<dl>
			<dt>Status</dt>
			<dd><StatusBadge status={current.run.status} /></dd>
			<dt>Control state</dt>
			<dd>{current.state}</dd>
			<dt>Script path</dt>
			<dd>{current.run.script_path}</dd>
			<dt>Concurrency</dt>
			<dd>{current.run.concurrency}</dd>
			<dt>Rate limit</dt>
			<dd>{current.run.rate_limit}</dd>
			<dt>Started at</dt>
			<dd>{new Date(current.run.started_at * 1000).toLocaleString()}</dd>
		</dl>
		<ControlBar onSignal={handleSignal} />
	</article>
{:else}
	<p>No run in progress.</p>
{/if}
