<script lang="ts">
  import NotImplemented from '$lib/components/NotImplemented.svelte';

  let { data } = $props<{ data: { implemented: boolean; id: string } }>();
</script>

<h1>Run {data.id}</h1>

{#if data.implemented}
  <!-- future: run detail (requests + metric windows + CSV download) -->
{:else}
  <NotImplemented feature="Run detail" endpoint="GET /api/runs/:id" />
{/if}
