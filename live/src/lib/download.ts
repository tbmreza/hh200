// Fetches `url` and triggers a browser download as `filename`.
export async function downloadFile(url: string, filename: string): Promise<void> {
	const res = await fetch(url);
	if (!res.ok) throw new Error(`download failed: ${url} returned HTTP ${res.status}`);
	const blob = await res.blob();
	const objectUrl = URL.createObjectURL(blob);

	const a = document.createElement('a');
	a.href = objectUrl;
	a.download = filename;
	document.body.append(a);
	a.click();
	a.remove();

	// Deferred so the browser has a chance to start the download first.
	setTimeout(() => URL.revokeObjectURL(objectUrl), 1_000);
}
