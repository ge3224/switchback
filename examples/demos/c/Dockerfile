# Stage 1: Deno builder for the frontend
# Uses Deno + esbuild for fast, dependency-free bundling
FROM docker.io/denoland/deno:2.1.4 AS deno-builder

WORKDIR /build

# Copy Switchback source
COPY src ./src
COPY tsconfig.json ./

# Copy demo app file
COPY examples/demos/c/app.ts ./examples/demos/c/app.ts

# Build with Deno (uses esbuild from npm via Deno's npm compatibility)
# This is MUCH faster than npm install + vite build - no node_modules needed!
WORKDIR /build/examples/demos/c
RUN mkdir -p dist
RUN deno run -A npm:esbuild app.ts --bundle --outfile=dist/app.js --format=esm --platform=browser

# Stage 2: C builder
FROM alpine:3.19 AS c-builder

# Install GCC and build tools
RUN apk add --no-cache gcc musl-dev

WORKDIR /build

# Copy C source
COPY examples/demos/c/server.c .

# Build C server
RUN gcc -o server server.c -pthread -O2 -Wall

# Stage 3: Runtime
FROM alpine:3.19

WORKDIR /app

# Copy built JS bundle
COPY --from=deno-builder /build/examples/demos/c/dist/app.js ./dist/app.js

# Copy C binary
COPY --from=c-builder /build/server ./server

# Expose port
EXPOSE 8000

# Run C server
CMD ["./server"]
