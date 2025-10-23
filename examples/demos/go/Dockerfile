# Stage 1: Deno builder for the frontend
# Uses Deno + esbuild for fast, dependency-free bundling
FROM docker.io/denoland/deno:2.1.4 AS deno-builder

WORKDIR /build

# Copy Switchback source
COPY src ./src
COPY tsconfig.json ./

# Copy demo app file
COPY examples/demos/go/app.ts ./examples/demos/go/app.ts

# Build with Deno (uses esbuild from npm via Deno's npm compatibility)
# This is MUCH faster than npm install + vite build - no node_modules needed!
WORKDIR /build/examples/demos/go
RUN mkdir -p dist
RUN deno run -A npm:esbuild app.ts --bundle --outfile=dist/app.js --format=esm --platform=browser

# Go builder stage
FROM golang:1.21-alpine AS go-builder

WORKDIR /build

# Copy Go source
COPY examples/demos/go/server.go .

# Build Go server (static binary)
RUN go build -o server server.go

# Runtime stage
FROM alpine:3.19

WORKDIR /app

# Copy built JS bundle
COPY --from=deno-builder /build/examples/demos/go/dist/app.js ./dist/app.js

# Copy Go binary
COPY --from=go-builder /build/server ./server

# Expose port
EXPOSE 8000

# Run Go server
CMD ["./server"]
