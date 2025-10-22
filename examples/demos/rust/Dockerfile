# Stage 1: Node.js builder for the frontend
FROM node:20-slim AS node-builder

# Install pnpm
RUN npm install -g pnpm

WORKDIR /build

# Copy root package files for dependencies
COPY package.json pnpm-lock.yaml ./

# Install pnpm and root dependencies
RUN pnpm install --frozen-lockfile

# Copy Switchback source
COPY src ./src
COPY tsconfig.json ./

# Copy demo files maintaining structure: examples/demos/rust/
COPY examples/demos/rust/app.ts ./examples/demos/rust/app.ts
COPY examples/demos/rust/vite.config.ts ./examples/demos/rust/vite.config.ts
COPY examples/demos/rust/package.json ./examples/demos/rust/package.json
COPY examples/demos/rust/.npmrc ./examples/demos/rust/.npmrc

# Build bundled app (includes Switchback)
WORKDIR /build/examples/demos/rust
RUN pnpm install --reporter=silent 2>&1 | grep -v "Ignored build scripts" || true
RUN pnpm build:js

# Stage 2: Rust builder for the server
FROM rust:1.82-slim AS rust-builder

WORKDIR /build

# Copy Rust server source and cargo manifest
COPY examples/demos/rust/Cargo.toml ./
COPY examples/demos/rust/server.rs ./
COPY examples/demos/rust/src ./src

# Build Rust server (release mode for performance with server features)
RUN cargo build --release --bin rust-recipe --features server

# Stage 3: Runtime
FROM debian:bookworm-slim

WORKDIR /app

# Copy built JS bundle
COPY --from=node-builder /build/examples/demos/rust/dist/app.js ./public/dist/app.js

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
