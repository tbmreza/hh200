<script lang="ts">
	import type { RunSignal } from '$lib/types';

	let {
		onSignal,
		disabled = false,
	}: { onSignal: (signal: RunSignal) => void; disabled?: boolean } = $props();

	const signals: RunSignal[] = ['pause', 'resume', 'stop'];
</script>

<div class="controls">
	{#each signals as signal (signal)}
		<button {disabled} onclick={() => onSignal(signal)}>{signal}</button>
	{/each}
</div>

<style>
	.controls {
		display: flex;
		gap: 0.5rem;
	}
</style>
