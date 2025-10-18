# JS builder stage
FROM node:20-alpine AS js-builder

WORKDIR /build

# Copy root package files for dependencies
COPY package.json pnpm-lock.yaml ./

# Install pnpm and root dependencies
RUN npm install -g pnpm && pnpm install --frozen-lockfile

# Copy Switchback source
COPY src ./src
COPY tsconfig.json ./

# Copy recipe files maintaining structure: examples/demos/rust/
COPY examples/demos/rust/app.ts ./examples/demos/rust/app.ts
COPY examples/demos/rust/vite.config.ts ./examples/demos/rust/vite.config.ts
COPY examples/demos/rust/package.json ./examples/demos/rust/package.json
COPY examples/demos/rust/.npmrc ./examples/demos/rust/.npmrc

# Build bundled app (includes Switchback)
WORKDIR /build/examples/demos/rust
RUN pnpm install --reporter=silent 2>&1 | grep -v "Ignored build scripts" || true
RUN pnpm build:js

# Rust server builder stage
FROM rust:1.82-slim AS rust-builder

WORKDIR /build

# Copy Rust server source and cargo manifest
COPY examples/demos/rust/server.rs .
COPY examples/demos/rust/Cargo.toml .
COPY examples/demos/rust/src ./src

# Build Rust server (release mode for performance with server features)
RUN cargo build --release --bin rust-recipe --features server

# Runtime stage
FROM debian:bookworm-slim

WORKDIR /app

# Copy built JS bundle
COPY --from=js-builder /build/examples/demos/rust/dist/app.js ./dist/app.js

# Copy Rust binary
COPY --from=rust-builder /build/target/release/rust-recipe ./rust-recipe

# Expose port
EXPOSE 8000

# Run Rust server
CMD ["./rust-recipe"]
