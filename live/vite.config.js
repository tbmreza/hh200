import { sveltekit } from '@sveltejs/kit/vite';
import { defineConfig } from 'vite';

export default defineConfig({
  plugins: [sveltekit()],

  // Development mode HMR.
  server: {
    proxy: {
      '/api': 'http://localhost:8089'
    }
  }
});
