# Pure Deno build - no Node.js required!
FROM docker.io/denoland/deno:2.1.4 AS builder

WORKDIR /build

# Copy Switchback source
COPY src ./src
COPY tsconfig.json ./

# Copy recipe files
COPY examples/recipes/deno/app.ts ./examples/recipes/deno/app.ts
COPY examples/recipes/deno/shared-types.ts ./examples/recipes/deno/shared-types.ts
COPY examples/recipes/deno/deno.json ./examples/recipes/deno/deno.json

# Build with Deno (uses esbuild from npm via Deno's npm compatibility)
WORKDIR /build/examples/recipes/deno
RUN mkdir -p dist
RUN deno task build

# Runtime stage
FROM docker.io/denoland/deno:2.1.4

WORKDIR /app

# Copy built JS bundle
COPY --from=builder /build/examples/recipes/deno/dist/app.js ./dist/app.js

# Copy Deno server and shared types
COPY examples/recipes/deno/server.ts ./server.ts
COPY examples/recipes/deno/shared-types.ts ./shared-types.ts

# Expose port
EXPOSE 8000

# Run Deno server
CMD ["deno", "run", "--allow-net", "--allow-read", "server.ts"]
