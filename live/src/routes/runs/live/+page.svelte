<script lang="ts">
  import NotImplemented from '$lib/components/NotImplemented.svelte';

  let { data } = $props<{ data: { implemented: boolean } }>();
</script>

<div class="page-heading">
  <div>
    <p class="eyebrow">Real-time observability</p>
    <h1>Live monitor</h1>
    <p class="lede">Keep an eye on active traffic, throughput, and worker health from one place.</p>
  </div>
  <span class="status-chip"><span class="dot"></span> Monitoring</span>
</div>

<section class="metrics" aria-label="Live run metrics">
  <article class="metric-card">
    <span class="metric-label">Active runs</span>
    <strong>—</strong>
    <small>Waiting for live data</small>
  </article>
  <article class="metric-card">
    <span class="metric-label">Requests / sec</span>
    <strong>—</strong>
    <small>Live stream unavailable</small>
  </article>
  <article class="metric-card accent">
    <span class="metric-label">System health</span>
    <strong>Ready</strong>
    <small>Dashboard is connected</small>
  </article>
</section>

{#if data.implemented}
  <!-- future: live run + control bar -->
{:else}
  <NotImplemented feature="Live run data" endpoint="GET /api/runs/live" />
{/if}

<section class="activity-card">
  <div>
    <p class="eyebrow">Activity feed</p>
    <h2>Recent events</h2>
  </div>
  <p class="empty-feed">New run events will appear here as soon as the live endpoint is enabled.</p>
</section>

<style>
  .page-heading {
    display: flex;
    align-items: flex-start;
    justify-content: space-between;
    gap: 1.5rem;
    margin-bottom: 2rem;
  }

  .eyebrow {
    margin: 0 0 0.55rem;
    color: #7c3aed;
    font-size: 0.68rem;
    font-weight: 800;
    letter-spacing: 0.13em;
    text-transform: uppercase;
  }

  .lede {
    max-width: 35rem;
    margin: 0.75rem 0 0;
    color: #64748b;
    font-size: 0.95rem;
    line-height: 1.6;
  }

  .status-chip {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    padding: 0.45rem 0.7rem;
    border: 1px solid #dbeafe;
    border-radius: 999px;
    background: #eff6ff;
    color: #2563eb;
    font-size: 0.72rem;
    font-weight: 700;
    white-space: nowrap;
  }

  .dot {
    width: 0.45rem;
    height: 0.45rem;
    border-radius: 50%;
    background: #60a5fa;
  }

  .metrics {
    display: grid;
    grid-template-columns: repeat(3, 1fr);
    gap: 1rem;
    margin-bottom: 1.5rem;
  }

  .metric-card,
  .activity-card {
    border: 1px solid #e6eaf1;
    border-radius: 1rem;
    background: white;
    box-shadow: 0 10px 28px #17203308;
  }

  .metric-card {
    min-height: 140px;
    padding: 1.25rem;
  }

  .metric-card.accent {
    border-color: #ddd6fe;
    background: linear-gradient(145deg, #faf5ff, #ffffff);
  }

  .metric-label,
  .metric-card small {
    display: block;
    color: #64748b;
    font-size: 0.78rem;
  }

  .metric-card strong {
    display: block;
    margin: 0.65rem 0 0.3rem;
    color: #172033;
    font-size: 1.65rem;
    letter-spacing: -0.04em;
  }

  .activity-card {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 1rem;
    padding: 1.35rem 1.5rem;
  }

  .activity-card h2 {
    margin: 0;
  }

  .empty-feed {
    margin: 0;
    color: #94a3b8;
    font-size: 0.82rem;
    text-align: right;
  }

  @media (max-width: 700px) {
    .page-heading,
    .activity-card {
      display: block;
    }

    .status-chip {
      display: inline-flex;
      margin-top: 1rem;
    }

    .metrics {
      grid-template-columns: 1fr;
    }

    .empty-feed {
      margin-top: 1rem;
      text-align: left;
    }
  }
</style>
