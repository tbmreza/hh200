<script lang="ts">
	import type { Run } from '$lib/types';
	import RunTable from '$lib/components/RunTable.svelte';

	let { data } = $props<{ data: { runs: Run[] } }>();
</script>

<h1>Runs</h1>

<RunTable runs={data.runs} />
