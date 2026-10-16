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

// Live control state mirrors the backend's `RunState` (Running/Paused/Stopped),
// distinct from the persisted `status` lifecycle above.
export type RunControlState = 'running' | 'paused' | 'stopped';

export type RunSignal = 'pause' | 'resume' | 'stop';

export type HeaderDirection = 'request' | 'response';

export type RequestHeader = {
	id: number;
	direction: HeaderDirection;
	name: string;
	value: string;
};

export type RequestBody = {
	id: number;
	direction: HeaderDirection;
	content: string | null; // Prisma `Bytes` serializes to a base64 string over JSON
	truncated: boolean;
};

export type Request = {
	id: number;
	seq: number;
	sent_at: number;
	duration_ms: number | null;
	method: string;
	url: string;
	status_code: number | null;
	error: string | null;
	bytes_in: number | null;
	bytes_out: number | null;
	worker_id: number | null;
	headers: RequestHeader[];
	bodies: RequestBody[];
};

export type MetricWindow = {
	id: number;
	run_id: number;
	endpoint: string;
	start: string; // ISO 8601 datetime
	end: string; // ISO 8601 datetime
	count: number;
	fail_count: number;
	min_latency: number;
	max_latency: number;
	avg_latency: number;
	rps: number;
};

export type RunDetail = {
	run: Run;
	requests: Request[];
	metric_windows: MetricWindow[];
};

export type LiveRun = {
	run: Run;
	state: RunControlState;
};

export type SseEvent =
	| { kind: 'NoNews' }
	| { kind: 'NewRow'; run: Run }
	| { kind: 'UpdateRow'; id: number; patch: Partial<Run> };
