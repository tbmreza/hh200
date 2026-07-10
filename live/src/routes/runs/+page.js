export const prerender = false;

export async function load({ fetch }) {
  const res = await fetch('/api/runs');
  const json = await res.json();
  return { runs: json.runs };
}
