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
    border: 1px dashed #9ca3af;
    border-radius: 0.5rem;
    padding: 1rem;
    background: #f9fafb;
    color: #6b7280;
  }
  code {
    background: #e5e7eb;
    padding: 0.1rem 0.3rem;
    border-radius: 0.25rem;
  }
</style>
