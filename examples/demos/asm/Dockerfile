# JS builder stage (uses native architecture - JS bundling is arch-independent)
FROM node:20-alpine AS js-builder

WORKDIR /build

# Copy root package files for dependencies
COPY package.json pnpm-lock.yaml ./

# Install pnpm and root dependencies
RUN npm install -g pnpm && pnpm install --frozen-lockfile

# Copy Switchback source
COPY src ./src
COPY tsconfig.json ./

# Copy demo files maintaining structure: examples/demos/asm/
COPY examples/demos/asm/app.ts ./examples/demos/asm/app.ts
COPY examples/demos/asm/vite.config.ts ./examples/demos/asm/vite.config.ts
COPY examples/demos/asm/package.json ./examples/demos/asm/package.json
COPY examples/demos/asm/.npmrc ./examples/demos/asm/.npmrc

# Build bundled app (includes Switchback)
WORKDIR /build/examples/demos/asm
RUN pnpm install --reporter=silent 2>&1 | grep -v "Ignored build scripts" || true
RUN pnpm build

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
COPY --from=js-builder /build/examples/demos/asm/dist/app.js ./dist/app.js

# Copy x86-64 binary
COPY --from=asm-builder /build/server ./server

# Expose port
EXPOSE 8000

# Run assembly server
CMD ["./server"]
