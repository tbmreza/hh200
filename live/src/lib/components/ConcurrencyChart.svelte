<script lang="ts">
	import type { ConcurrencyPoint } from '$lib/ramp';

	let { points }: { points: ConcurrencyPoint[] } = $props();

	const W = 600;
	const H = 150;
	const PAD = { top: 10, right: 10, bottom: 22, left: 38 };

	const maxActive = $derived(Math.max(1, ...points.map((p) => p.active)));
	const tMin = $derived(points[0]?.t ?? 0);
	const tMax = $derived(points[points.length - 1]?.t ?? 1);

	const x = (t: number) =>
		PAD.left + ((t - tMin) / Math.max(1e-9, tMax - tMin)) * (W - PAD.left - PAD.right);
	const y = (a: number) => PAD.top + (1 - a / maxActive) * (H - PAD.top - PAD.bottom);

	// A step path: `points[i].active` holds from `points[i].t` to `points[i+1].t`.
	const path = $derived.by(() => {
		if (points.length === 0) return '';
		const first = points[0];
		if (!first) return '';
		let d = `M ${x(first.t)} ${y(first.active)}`;
		for (let i = 1; i < points.length; i++) {
			const prev = points[i - 1];
			const cur = points[i];
			if (!prev || !cur) continue;
			d += ` L ${x(cur.t)} ${y(prev.active)}`;
			d += ` L ${x(cur.t)} ${y(cur.active)}`;
		}
		return d;
	});
</script>

{#if points.length === 0}
	<p class="empty">No concurrency data.</p>
{:else}
	<svg viewBox={`0 0 ${W} ${H}`} role="img" aria-label="Concurrency over time">
		<path class="area" d={`${path} L ${x(tMax)} ${y(0)} L ${x(tMin)} ${y(0)} Z`} />
		<path class="line" d={path} />
		<line class="axis" x1={PAD.left} y1={y(0)} x2={W - PAD.right} y2={y(0)} />
		<text class="label" x={PAD.left - 6} y={y(maxActive)} text-anchor="end">{maxActive}</text>
		<text class="label" x={x(tMax)} y={H - 6} text-anchor="end">{Math.round(tMax)}s</text>
	</svg>
{/if}

<style>
	svg {
		display: block;
		width: 100%;
		height: auto;
	}
	.line {
		fill: none;
		stroke: #7c3aed;
		stroke-width: 2;
	}
	.area {
		fill: #7c3aed18;
		stroke: none;
	}
	.axis {
		stroke: #e5e7eb;
		stroke-width: 1;
	}
	.label {
		fill: #64748b;
		font-size: 10px;
	}
	.empty {
		color: #94a3b8;
		font-size: 0.82rem;
	}
</style>
