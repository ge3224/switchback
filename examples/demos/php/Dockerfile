# Stage 1: Deno builder for the frontend
# Uses Deno + esbuild for fast, dependency-free bundling
FROM docker.io/denoland/deno:2.1.4 AS deno-builder

WORKDIR /build

# Copy Switchback source
COPY src ./src
COPY tsconfig.json ./

# Copy demo app file
COPY examples/demos/php/app.ts ./examples/demos/php/app.ts

# Build with Deno (uses esbuild from npm via Deno's npm compatibility)
# This is MUCH faster than npm install + vite build - no node_modules needed!
WORKDIR /build/examples/demos/php
RUN mkdir -p dist
RUN deno run -A npm:esbuild app.ts --bundle --outfile=dist/app.js --format=esm --platform=browser

# Runtime stage
FROM php:8.3-cli

WORKDIR /app

# Copy built bundle
COPY --from=deno-builder /build/examples/demos/php/dist/app.js ./dist/app.js

# Copy PHP backend and entrypoint
COPY examples/demos/php/index.php .
COPY examples/demos/php/entrypoint.sh .
RUN chmod +x entrypoint.sh

# Expose port
EXPOSE 8000

# Use entrypoint script for proper signal handling
CMD ["./entrypoint.sh"]
