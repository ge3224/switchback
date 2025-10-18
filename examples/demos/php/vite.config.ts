import { defineConfig } from "vite";
import { resolve } from "path";

export default defineConfig({
  build: {
    outDir: "dist",
    emptyOutDir: true,
    lib: {
      entry: resolve(process.cwd(), "app.ts"),
      name: "App",
      fileName: "app",
      formats: ["es"],
    },
    rollupOptions: {
      output: {
        entryFileNames: "app.js",
      },
    },
  },
});
