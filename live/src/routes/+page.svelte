<script lang="ts">
	import { onMount } from 'svelte';
	import { Chart } from 'chart.js/auto';
	import type { Run } from '$lib/types';
	// import { source } from 'sveltekit-sse';

	// const balue = source('/sse').select('message')

	let { data } = $props<{ runs: Run[]; chartData: { year: number; count: number }[] }>();
	let runs = $derived(data.runs ?? []);
	let canvas: HTMLCanvasElement;

	onMount(() => {
		new Chart(canvas, {
			type: 'bar',
			data: {
				labels: data.chartData.map(row => row.year),
				datasets: [
					{
						label: 'Acquisitions by year',
						data: data.chartData.map(row => row.count),
					},
				],
			},
		});
	});

	async function downloadCsv(runId: number) {
		const res = await fetch(`/api/report/${runId}`);
		if (!res.ok) throw new Error(`report ${runId}: HTTP ${res.status}`);
		const blob = await res.blob();
		const url = URL.createObjectURL(blob);
		const a = document.createElement('a');
		a.href = url;
		a.download = 'stats_history.csv';
		a.click();
		URL.revokeObjectURL(url);
	}
</script>

<section>
	<canvas bind:this={canvas}></canvas>
</section>

<h1>goal: serve from scotty</h1>
<button onclick={async () => await fetch('/api/runs')}>stop2</button>
<button>yatta</button>

<p>Visit <a href="https://svelte.dev/docs/kit">svelte.dev/docs/kit</a> to read the documentation</p>

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
    {#each runs as run}
      <tr>
        <td><a href="/runs/{run.name}">{run.name}</a></td>
        <td>{run.status}</td>
        <td>{run.script_path}</td>
        <td>{run.concurrency}</td>
        <td>{run.rate_limit}</td>
        <td><button onclick={() => downloadCsv(run.id)}>download csv</button></td>
      </tr>
    {/each}
  </tbody>
</table>
