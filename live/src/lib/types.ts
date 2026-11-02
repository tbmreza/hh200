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

export type RunDetail = {
	run: Run;
	requests: Request[];
};

export type LiveRun = {
	run: Run | null;
	state: RunControlState;
};

export type SseEvent =
	| { kind: 'NoNews' }
	| { kind: 'NewRow'; run: Run }
	| { kind: 'UpdateRow'; id: number; patch: Partial<Run> };

// A value the backend can compute now, or a gap it cannot fill yet.
export type Foreseeable<T> =
	| { known: true; value: T }
	| { known: false; reason: string };

export type RunRequest = {
	name: string;
	script: string;
	nvu: number;
	duration: number;
};

export type ScriptError = {
	line: number;
	column: number;
	message: string;
};

// Response of POST /api/run/plan. Only the validation fields are guaranteed;
// the foresee fields are absent when the script is invalid.
export type RunPlan = {
	scriptValid: boolean;
	errors: ScriptError[];
	inFlightCeiling?: number;
	wallClock?: string;
	rateLimit?: Foreseeable<number>;
	totalRequests?: Foreseeable<number>;
	rateLimitSource?: string;
	callItems?: number;
};

export type StartRunResponse = {
	id: number;
};
