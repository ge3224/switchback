# Stage 1: Deno builder for the frontend
# Uses Deno + esbuild for fast, dependency-free bundling
FROM docker.io/denoland/deno:2.1.4 AS deno-builder

WORKDIR /build

# Copy Switchback source
COPY src ./src
COPY tsconfig.json ./

# Copy demo app file
COPY examples/demos/rust/app.ts ./examples/demos/rust/app.ts

# Build with Deno (uses esbuild from npm via Deno's npm compatibility)
# This is MUCH faster than npm install + vite build - no node_modules needed!
WORKDIR /build/examples/demos/rust
RUN mkdir -p dist
RUN deno run -A npm:esbuild app.ts --bundle --outfile=dist/app.js --format=esm --platform=browser

# Stage 2: Rust builder for the server
FROM rust:1.82-slim AS rust-builder

WORKDIR /build

# Copy Rust server source and cargo manifest
COPY examples/demos/rust/Cargo.toml ./
COPY examples/demos/rust/server.rs ./

# Build Rust server (release mode for performance)
RUN cargo build --release --bin rust-recipe

# Stage 3: Runtime
FROM debian:bookworm-slim

WORKDIR /app

# Copy built JS bundle
COPY --from=deno-builder /build/examples/demos/rust/dist/app.js ./public/dist/app.js

# Copy Rust binary
COPY --from=rust-builder /build/target/release/rust-recipe ./rust-recipe

# Copy sample images
COPY examples/demos/rust/public/samples ./public/samples

# Create upload directories
RUN mkdir -p public/uploads/original public/uploads/thumb public/uploads/medium public/uploads/grayscale

# Expose port
EXPOSE 8000

# Run the server
CMD ["./rust-recipe"]
