# Stage 1: Deno builder for the frontend
# Uses Deno + esbuild for fast, dependency-free bundling
FROM docker.io/denoland/deno:2.1.4 AS deno-builder

WORKDIR /build

# Copy Switchback source
COPY src ./src
COPY tsconfig.json ./

# Copy demo app file
COPY examples/demos/zig/app.ts ./examples/demos/zig/app.ts

# Build with Deno (uses esbuild from npm via Deno's npm compatibility)
# This is MUCH faster than npm install + vite build - no node_modules needed!
WORKDIR /build/examples/demos/zig
RUN mkdir -p dist
RUN deno run -A npm:esbuild app.ts --bundle --outfile=dist/app.js --format=esm --platform=browser

# Stage 2: Zig builder for the server
FROM alpine:3.19 AS zig-builder

# Install Zig
RUN apk add --no-cache wget tar xz \
    && wget -q https://ziglang.org/download/0.13.0/zig-linux-x86_64-0.13.0.tar.xz \
    && tar -xf zig-linux-x86_64-0.13.0.tar.xz \
    && mv zig-linux-x86_64-0.13.0 /usr/local/zig \
    && ln -s /usr/local/zig/zig /usr/local/bin/zig \
    && rm zig-linux-x86_64-0.13.0.tar.xz

WORKDIR /build

# Copy Zig source
COPY examples/demos/zig/server.zig .

# Build Zig server
RUN zig build-exe server.zig -O ReleaseFast

# Stage 3: Runtime
FROM alpine:3.19

WORKDIR /app

# Copy built JS bundle
COPY --from=deno-builder /build/examples/demos/zig/dist/app.js ./dist/app.js

# Copy Zig binary
COPY --from=zig-builder /build/server ./server

# Expose port
EXPOSE 8000

# Run Zig server
CMD ["./server"]
