<script>
	import { onMount } from 'svelte';
	import { Chart } from 'chart.js/auto';
	// import { source } from 'sveltekit-sse';

	// const balue = source('/sse').select('message')

	let { data } = $props();
	let runs = $derived(data.runs ?? []);
	let canvas;

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
</script>

<section>
	<canvas bind:this={canvas} />
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
      </tr>
    {/each}
  </tbody>
</table>
