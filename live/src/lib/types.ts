export type Run = {
	id: number;
	name: string;
	status: string;
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
