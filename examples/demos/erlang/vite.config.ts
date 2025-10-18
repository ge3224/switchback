import { defineConfig } from 'vite';
import { resolve } from 'path';
import { fileURLToPath } from 'url';
import { dirname } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

export default defineConfig({
  build: {
    outDir: 'dist',
    lib: {
      entry: resolve(__dirname, 'app.ts'),
      name: 'SwitchbackErlangApp',
      formats: ['es'],
      fileName: () => 'app.js',
    },
    rollupOptions: {
      external: [],
    },
    sourcemap: true,
  },
  resolve: {
    alias: {
      '../../../src': '/workspace/src',
    },
  },
});
