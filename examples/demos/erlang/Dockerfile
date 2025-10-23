# Multi-stage Dockerfile for Erlang + Switchback

# Stage 1: Deno builder for the frontend
# Uses Deno + esbuild for fast, dependency-free bundling
FROM docker.io/denoland/deno:2.1.4 AS deno-builder

WORKDIR /build

# Copy Switchback source
COPY src ./src
COPY tsconfig.json ./

# Copy demo app file
COPY examples/demos/erlang/app.ts ./examples/demos/erlang/app.ts

# Build with Deno (uses esbuild from npm via Deno's npm compatibility)
# This is MUCH faster than npm install + vite build - no node_modules needed!
WORKDIR /build/examples/demos/erlang
RUN mkdir -p dist
RUN deno run -A npm:esbuild app.ts --bundle --outfile=dist/app.js --format=esm --platform=browser

# Stage 2: Build Erlang application
FROM docker.io/library/erlang:26-alpine AS erlang-builder

WORKDIR /app

# Install rebar3
RUN apk add --no-cache git && \
    wget https://s3.amazonaws.com/rebar3/rebar3 && \
    chmod +x rebar3 && \
    mv rebar3 /usr/local/bin/

# Copy Erlang source
COPY examples/demos/erlang/rebar.config ./
COPY examples/demos/erlang/src ./src

# Fetch dependencies and compile
RUN rebar3 get-deps
RUN rebar3 compile

# Stage 3: Runtime
FROM docker.io/library/erlang:26-alpine

WORKDIR /app

# Install runtime dependencies
RUN apk add --no-cache bash

# Copy compiled Erlang application
COPY --from=erlang-builder /app/_build /app/_build
COPY --from=erlang-builder /app/rebar.config /app/rebar.config
COPY --from=erlang-builder /app/src /app/src

# Copy built JavaScript bundle
COPY --from=deno-builder /build/examples/demos/erlang/dist /app/dist

# Install rebar3 for runtime
RUN apk add --no-cache git && \
    wget https://s3.amazonaws.com/rebar3/rebar3 && \
    chmod +x rebar3 && \
    mv rebar3 /usr/local/bin/

EXPOSE 8000

# Start the Erlang application
CMD ["rebar3", "shell", "--apps", "switchback_chat"]
