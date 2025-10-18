FROM node:20-alpine AS builder

WORKDIR /build

# Copy root package files for dependencies
COPY package.json pnpm-lock.yaml ./

# Install pnpm and root dependencies
RUN npm install -g pnpm && pnpm install --frozen-lockfile

# Copy Switchback source
COPY src ./src
COPY tsconfig.json ./

# Copy recipe files maintaining structure: examples/demos/php/
COPY examples/demos/php/app.ts ./examples/demos/php/app.ts
COPY examples/demos/php/vite.config.ts ./examples/demos/php/vite.config.ts
COPY examples/demos/php/package.json ./examples/demos/php/package.json
COPY examples/demos/php/.npmrc ./examples/demos/php/.npmrc

# Build bundled app (includes Switchback)
WORKDIR /build/examples/demos/php
RUN pnpm install --reporter=silent 2>&1 | grep -v "Ignored build scripts" || true
RUN pnpm build

# Runtime stage
FROM php:8.3-cli

WORKDIR /app

# Copy built bundle
COPY --from=builder /build/examples/demos/php/dist/app.js ./dist/app.js

# Copy PHP backend and entrypoint
COPY examples/demos/php/index.php .
COPY examples/demos/php/entrypoint.sh .
RUN chmod +x entrypoint.sh

# Expose port
EXPOSE 8000

# Use entrypoint script for proper signal handling
CMD ["./entrypoint.sh"]
