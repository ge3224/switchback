FROM node:20-alpine AS js-builder

WORKDIR /build

# Copy root package files for dependencies
COPY package.json pnpm-lock.yaml ./

# Install pnpm and root dependencies
RUN npm install -g pnpm && pnpm install --frozen-lockfile

# Copy Switchback source
COPY src ./src
COPY tsconfig.json ./

# Copy recipe files maintaining structure: examples/demos/c/
COPY examples/demos/c/app.ts ./examples/demos/c/app.ts
COPY examples/demos/c/vite.config.ts ./examples/demos/c/vite.config.ts
COPY examples/demos/c/package.json ./examples/demos/c/package.json
COPY examples/demos/c/.npmrc ./examples/demos/c/.npmrc

# Build bundled app (includes Switchback)
WORKDIR /build/examples/demos/c
RUN pnpm install --reporter=silent 2>&1 | grep -v "Ignored build scripts" || true
RUN pnpm build

# C builder stage
FROM alpine:3.19 AS c-builder

# Install GCC and build tools
RUN apk add --no-cache gcc musl-dev

WORKDIR /build

# Copy C source
COPY examples/demos/c/server.c .

# Build C server
RUN gcc -o server server.c -pthread -O2 -Wall

# Runtime stage
FROM alpine:3.19

WORKDIR /app

# Copy built JS bundle
COPY --from=js-builder /build/examples/demos/c/dist/app.js ./dist/app.js

# Copy C binary
COPY --from=c-builder /build/server ./server

# Expose port
EXPOSE 8000

# Run C server
CMD ["./server"]
