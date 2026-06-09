export const prerender = false;

export async function load({ fetch, params }) {
  const res = await fetch('/runs');
  const json = await res.json();
  const run = json.runs.find(r => r.name === params.id);
  return { run };
}
