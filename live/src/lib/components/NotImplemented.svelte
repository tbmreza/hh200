<script lang="ts">
  let { feature, endpoint }: { feature: string; endpoint: string } = $props();
</script>

<section class="not-implemented">
  <h2>Not implemented yet</h2>
  <p>
    {feature} is not available yet: it depends on <code>{endpoint}</code>, which
    the backend does not implement.
  </p>
</section>

<style>
  .not-implemented {
    display: flex;
    align-items: flex-start;
    gap: 0.9rem;
    margin: 0 0 1.5rem;
    padding: 1.1rem 1.25rem;
    border: 1px solid #fed7aa;
    border-radius: 0.9rem;
    background: #fffaf5;
    color: #9a3412;
  }
  .not-implemented::before {
    content: '!';
    display: grid;
    flex: 0 0 1.5rem;
    width: 1.5rem;
    height: 1.5rem;
    place-items: center;
    border-radius: 50%;
    background: #ffedd5;
    color: #c2410c;
    font-weight: 800;
  }
  h2 {
    margin: 0 0 0.35rem;
    color: #9a3412;
    font-size: 0.9rem;
  }
  p {
    margin: 0;
    font-size: 0.82rem;
    line-height: 1.5;
  }
  code {
    background: #ffedd5;
    padding: 0.1rem 0.3rem;
    border-radius: 0.25rem;
  }
</style>
