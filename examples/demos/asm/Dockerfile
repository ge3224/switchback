# Stage 1: Deno builder for the frontend
# Uses Deno + esbuild for fast, dependency-free bundling
FROM docker.io/denoland/deno:2.1.4 AS deno-builder

WORKDIR /build

# Copy Switchback source
COPY src ./src
COPY tsconfig.json ./

# Copy demo app file
COPY examples/demos/asm/app.ts ./examples/demos/asm/app.ts

# Build with Deno (uses esbuild from npm via Deno's npm compatibility)
# This is MUCH faster than npm install + vite build - no node_modules needed!
WORKDIR /build/examples/demos/asm
RUN mkdir -p dist
RUN deno run -A npm:esbuild app.ts --bundle --outfile=dist/app.js --format=esm --platform=browser

# x86-64 Assembly builder stage
FROM alpine:3.19 AS asm-builder

# Install binutils for x86-64 assembler and linker
RUN apk add --no-cache binutils

WORKDIR /build

# Copy assembly source
COPY examples/demos/asm/server.s .

# Assemble and link x86-64 assembly
RUN as -o server.o server.s && \
    ld -o server server.o

# Runtime stage - x86-64
FROM alpine:3.19

WORKDIR /app

# Copy built JS bundle
COPY --from=deno-builder /build/examples/demos/asm/dist/app.js ./dist/app.js

# Copy x86-64 binary
COPY --from=asm-builder /build/server ./server

# Expose port
EXPOSE 8000

# Run assembly server
CMD ["./server"]
