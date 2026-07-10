<script>
	import { Chart } from 'chart.js/auto';
	import { onMount } from 'svelte';

	let { data } = $props();
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
<button>yatta</button>

<p>Visit <a href="https://svelte.dev/docs/kit">svelte.dev/docs/kit</a> to read the documentation</p>
