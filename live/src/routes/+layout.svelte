<script lang="ts">
	import favicon from '$lib/assets/favicon.svg';

	let { children } = $props();
</script>

<svelte:head>
	<link rel="icon" href={favicon} />
</svelte:head>

<div class="app-shell">
	<aside class="sidebar">
		<a class="brand" href="/runs" aria-label="hh200 dashboard home">
			<span class="brand-mark">H</span>
			<span>
				<strong>hh200</strong>
				<small>Load workspace</small>
			</span>
		</a>

		<nav aria-label="Primary navigation">
			<p class="nav-label">Workspace</p>
			<a href="/runs" class="nav-link">
				<span class="nav-icon">▦</span>
				<span>Runs</span>
			</a>
			<a href="/runs/live" class="nav-link">
				<span class="nav-icon">◉</span>
				<span>Live monitor</span>
				<span class="nav-status">Soon</span>
			</a>
			<a href="/run" class="nav-link">
				<span class="nav-icon">＋</span>
				<span>New run</span>
			</a>
		</nav>

		<div class="sidebar-foot">
			<span class="pulse"></span>
			<span>Dashboard online</span>
		</div>
	</aside>

	<div class="content-shell">
		<header class="topbar">
			<div>
				<span class="topbar-kicker">Operations console</span>
				<span class="topbar-title">Performance workspace</span>
			</div>
			<span class="environment"><span class="pulse"></span> Local environment</span>
		</header>
		<main>
			{@render children()}
		</main>
	</div>
</div>

<style>
	:global(*) {
		box-sizing: border-box;
	}

	:global(html) {
		background: #f5f7fb;
	}

	:global(body) {
		margin: 0;
		color: #172033;
		font-family:
			Inter, ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
		-webkit-font-smoothing: antialiased;
	}

	:global(button),
	:global(input) {
		font: inherit;
	}

	:global(a) {
		color: inherit;
	}

	.app-shell {
		display: flex;
		min-height: 100vh;
	}

	.sidebar {
		display: flex;
		flex: 0 0 248px;
		flex-direction: column;
		padding: 1.75rem 1rem 1.25rem;
		background: #111827;
		color: #cbd5e1;
	}

	.brand {
		display: flex;
		align-items: center;
		gap: 0.75rem;
		padding: 0 0.75rem 2.5rem;
		color: white;
		text-decoration: none;
	}

	.brand-mark {
		display: grid;
		width: 2.25rem;
		height: 2.25rem;
		place-items: center;
		border-radius: 0.75rem;
		background: linear-gradient(135deg, #7c3aed, #4f46e5);
		font-size: 1.15rem;
		font-weight: 800;
		box-shadow: 0 8px 20px #4f46e544;
	}

	.brand strong,
	.brand small {
		display: block;
	}

	.brand strong {
		font-size: 1.05rem;
		letter-spacing: -0.02em;
	}

	.brand small {
		margin-top: 0.15rem;
		color: #94a3b8;
		font-size: 0.7rem;
	}

	.nav-label {
		margin: 0 0 0.65rem 0.75rem;
		color: #64748b;
		font-size: 0.68rem;
		font-weight: 700;
		letter-spacing: 0.12em;
		text-transform: uppercase;
	}

	.nav-link {
		display: flex;
		align-items: center;
		gap: 0.7rem;
		margin: 0.25rem 0;
		padding: 0.75rem;
		border-radius: 0.65rem;
		color: #94a3b8;
		font-size: 0.88rem;
		text-decoration: none;
		transition: background 150ms ease, color 150ms ease;
	}

	.nav-link:hover,
	.nav-link:focus-visible {
		background: #1e293b;
		color: white;
	}

	.nav-icon {
		width: 1.25rem;
		color: #818cf8;
		font-size: 1rem;
		text-align: center;
	}

	.nav-status {
		margin-left: auto;
		padding: 0.15rem 0.4rem;
		border: 1px solid #334155;
		border-radius: 999px;
		color: #64748b;
		font-size: 0.62rem;
	}

	.sidebar-foot {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		margin-top: auto;
		padding: 1rem 0.75rem 0;
		border-top: 1px solid #1e293b;
		color: #64748b;
		font-size: 0.72rem;
	}

	.pulse {
		display: inline-block;
		width: 0.45rem;
		height: 0.45rem;
		border-radius: 50%;
		background: #34d399;
		box-shadow: 0 0 0 4px #34d3991f;
	}

	.content-shell {
		min-width: 0;
		flex: 1;
	}

	.topbar {
		display: flex;
		align-items: center;
		justify-content: space-between;
		min-height: 74px;
		padding: 1rem clamp(1.25rem, 4vw, 3.5rem);
		border-bottom: 1px solid #e6eaf1;
		background: #ffffffcc;
	}

	.topbar-kicker,
	.topbar-title {
		display: block;
	}

	.topbar-kicker {
		margin-bottom: 0.2rem;
		color: #7c3aed;
		font-size: 0.65rem;
		font-weight: 800;
		letter-spacing: 0.12em;
		text-transform: uppercase;
	}

	.topbar-title {
		color: #475569;
		font-size: 0.8rem;
	}

	.environment {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		color: #64748b;
		font-size: 0.75rem;
	}

	:global(main) {
		width: min(1180px, 100%);
		margin: 0 auto;
		padding: clamp(1.5rem, 4vw, 3.25rem) clamp(1.25rem, 4vw, 3.5rem);
	}

	:global(h1) {
		margin: 0;
		color: #111827;
		font-size: clamp(1.8rem, 3vw, 2.5rem);
		letter-spacing: -0.045em;
	}

	:global(h2) {
		color: #1e293b;
		font-size: 1.05rem;
		letter-spacing: -0.02em;
	}

	@media (max-width: 700px) {
		.sidebar {
			flex-basis: 68px;
			align-items: center;
			padding-inline: 0.5rem;
		}

		.brand {
			padding-inline: 0;
		}

		.brand strong,
		.brand small,
		.nav-label,
		.nav-link span:not(.nav-icon),
		.sidebar-foot span:not(.pulse) {
			display: none;
		}

		.nav-link {
			justify-content: center;
			width: 48px;
		}

		.sidebar-foot {
			padding-inline: 0;
		}

		.topbar {
			padding-inline: 1.25rem;
		}

		.environment {
			display: none;
		}
	}
</style>
