import { defineConfig } from "vite";
import { resolve } from "path";
import dts from "vite-plugin-dts";

export default defineConfig({
  root: process.env.VITE_ROOT || ".",
  build: {
    lib: {
      entry: resolve(__dirname, "src/index.ts"),
      name: "Switchback",
      fileName: (format) => {
        const suffix = process.env.BUILD_UNMINIFIED ? "" : ".min";
        if (format === "es") return "index.js";
        if (format === "umd") return `switchback.umd${suffix}.js`;
        return `switchback.iife${suffix}.js`;
      },
      formats: ["es", "umd", "iife"],
    },
    minify: process.env.BUILD_UNMINIFIED ? false : "esbuild",
    rollupOptions: {
      external: [],
      output: {
        exports: "named",
      },
    },
  },
  plugins: [dts({ include: ["src/index.ts"] })],
  test: {
    globals: true,
    environment: "jsdom",
    coverage: {
      provider: "v8",
      reporter: ["text", "json", "html"],
      exclude: ["node_modules/", "dist/", "**/*.test.ts"],
    },
  },
});
