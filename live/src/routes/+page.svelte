<script>
	import { Chart } from 'chart.js/auto';
	import { onMount } from 'svelte';

	let canvas;

	onMount(() => {
		const hey = [
			{ year: 2009, count: 10 },
			{ year: 2010, count: 10 },
			{ year: 2011, count: 20 },
			{ year: 2012, count: 15 },
			{ year: 2013, count: 25 },
			{ year: 2014, count: 22 },
			{ year: 2015, count: 30 },
			{ year: 2016, count: 28 },
		];
		new Chart(canvas, {
			type: 'bar',
			data: {
				labels: hey.map(row => row.year),
				datasets: [
					{
						label: 'Acquisitions by year',
						data: hey.map(row => row.count),
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
<p>Visit <a href="https://svelte.dev/docs/kit">svelte.dev/docs/kit</a> to read the documentation</p>
