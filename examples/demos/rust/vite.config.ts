import { defineConfig } from "vite";
import { resolve } from "path";

export default defineConfig({
  build: {
    outDir: "dist",
    emptyOutDir: true,
    rollupOptions: {
      input: resolve(process.cwd(), "app.ts"),
      output: {
        entryFileNames: "app.js",
        format: "es",
        inlineDynamicImports: true,
      },
    },
  },
  publicDir: resolve(process.cwd(), "pkg"),
});
