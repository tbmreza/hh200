// `started_at` and `ended_at` are Unix epoch **seconds**, matching the
// Haskell backend's `unixepoch('now')` and the `* 1000` conversion in the UI.

export type RunStatus = 'running' | 'completed' | 'failed' | 'cancelled';

export type Run = {
	id: number;
	name: string;
	status: RunStatus;
	script_path: string;
	concurrency: number;
	rate_limit: number;
	started_at: number;
	ended_at: number | null;
	control_socket: string;
};

export type RunsResponse = {
	runs: Run[];
};
