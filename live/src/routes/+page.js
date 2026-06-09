export const prerender = false;

export async function load({ fetch }) {
  const res = await fetch('/runs');
  const json = await res.json();
  const chartData = json.runs.map(run => ({
    year: run.name.length,
    count: run.script_path.length
  }));
  return { chartData };
}
