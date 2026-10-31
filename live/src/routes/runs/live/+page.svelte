<script lang="ts">
  import NotImplemented from '$lib/components/NotImplemented.svelte';

  let { data } = $props<{ data: { implemented: boolean } }>();
</script>

<h1>Live Run</h1>

{#if data.implemented}
  <!-- future: live run + control bar -->
{:else}
  <NotImplemented feature="The live run view" endpoint="GET /api/runs/live" />
{/if}
