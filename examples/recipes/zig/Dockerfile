FROM node:20-alpine AS js-builder

WORKDIR /build

# Copy root package files for dependencies
COPY package.json pnpm-lock.yaml ./

# Install pnpm and root dependencies
RUN npm install -g pnpm && pnpm install --frozen-lockfile

# Copy Switchback source
COPY src ./src
COPY tsconfig.json ./

# Copy recipe files maintaining structure: examples/recipes/zig/
COPY examples/recipes/zig/app.ts ./examples/recipes/zig/app.ts
COPY examples/recipes/zig/vite.config.ts ./examples/recipes/zig/vite.config.ts
COPY examples/recipes/zig/package.json ./examples/recipes/zig/package.json
COPY examples/recipes/zig/.npmrc ./examples/recipes/zig/.npmrc

# Build bundled app (includes Switchback)
WORKDIR /build/examples/recipes/zig
RUN pnpm install --reporter=silent 2>&1 | grep -v "Ignored build scripts" || true
RUN pnpm build

# Zig builder stage
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
COPY examples/recipes/zig/server.zig .

# Build Zig server
RUN zig build-exe server.zig -O ReleaseFast

# Runtime stage
FROM alpine:3.19

WORKDIR /app

# Copy built JS bundle
COPY --from=js-builder /build/examples/recipes/zig/dist/app.js ./dist/app.js

# Copy Zig binary
COPY --from=zig-builder /build/server ./server

# Expose port
EXPOSE 8000

# Run Zig server
CMD ["./server"]
