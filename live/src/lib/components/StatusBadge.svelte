<script lang="ts">
	import type { RunStatus } from '$lib/types';

	let { status }: { status: RunStatus } = $props();
</script>

<span
	class="badge"
	class:running={status === 'running'}
	class:completed={status === 'completed'}
	class:failed={status === 'failed'}
	class:cancelled={status === 'cancelled'}
>
	{status}
</span>

<style>
	.badge {
		padding: 0.1rem 0.5rem;
		border-radius: 999px;
		font-size: 0.85em;
	}
	.running {
		background: #fff7e6;
		color: #b45309;
	}
	.completed {
		background: #ecfdf5;
		color: #047857;
	}
	.failed {
		background: #fef2f2;
		color: #b91c1c;
	}
	.cancelled {
		background: #f3f4f6;
		color: #4b5563;
	}
</style>
