// PICKUP stop button to write to unix socket; chart to read initially and then SSE sequence
import adapter from '@sveltejs/adapter-static';

/** @type {import('@sveltejs/kit').Config} */
const config = {
	kit: {
		adapter: adapter({
			fallback: '200.html'
		})
	}
};

export default config;
