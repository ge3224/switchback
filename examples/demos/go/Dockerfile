FROM node:20-alpine AS js-builder

WORKDIR /build

# Copy root package files for dependencies
COPY package.json pnpm-lock.yaml ./

# Install pnpm and root dependencies
RUN npm install -g pnpm && pnpm install --frozen-lockfile

# Copy Switchback source
COPY src ./src
COPY tsconfig.json ./

# Copy recipe files maintaining structure: examples/demos/go/
COPY examples/demos/go/app.ts ./examples/demos/go/app.ts
COPY examples/demos/go/vite.config.ts ./examples/demos/go/vite.config.ts
COPY examples/demos/go/package.json ./examples/demos/go/package.json
COPY examples/demos/go/.npmrc ./examples/demos/go/.npmrc

# Build bundled app (includes Switchback)
WORKDIR /build/examples/demos/go
RUN pnpm install --reporter=silent 2>&1 | grep -v "Ignored build scripts" || true
RUN pnpm build

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
COPY --from=js-builder /build/examples/demos/go/dist/app.js ./dist/app.js

# Copy Go binary
COPY --from=go-builder /build/server ./server

# Expose port
EXPOSE 8000

# Run Go server
CMD ["./server"]
