# Multi-stage Dockerfile for Erlang + Switchback

# Stage 1: Build JavaScript bundle
FROM node:20-alpine AS js-builder

WORKDIR /app

# Copy parent package.json for shared dependencies
COPY package.json /workspace/package.json
COPY pnpm-lock.yaml /workspace/pnpm-lock.yaml

# Install pnpm
RUN npm install -g pnpm

# Install dependencies from workspace root
WORKDIR /workspace
RUN pnpm install

# Copy demo files
WORKDIR /app
COPY examples/recipes/erlang/package.json examples/recipes/erlang/vite.config.ts examples/recipes/erlang/.npmrc ./
COPY examples/recipes/erlang/app.ts ./

# Copy Switchback source
COPY src /workspace/src

# Install demo dependencies
RUN pnpm install

# Build the client bundle
RUN pnpm build

# Stage 2: Build Erlang application
FROM docker.io/library/erlang:26-alpine AS erlang-builder

WORKDIR /app

# Install rebar3
RUN apk add --no-cache git && \
    wget https://s3.amazonaws.com/rebar3/rebar3 && \
    chmod +x rebar3 && \
    mv rebar3 /usr/local/bin/

# Copy Erlang source
COPY examples/recipes/erlang/rebar.config ./
COPY examples/recipes/erlang/src ./src

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
COPY --from=js-builder /app/dist /app/dist

# Install rebar3 for runtime
RUN apk add --no-cache git && \
    wget https://s3.amazonaws.com/rebar3/rebar3 && \
    chmod +x rebar3 && \
    mv rebar3 /usr/local/bin/

EXPOSE 8000

# Start the Erlang application
CMD ["rebar3", "shell", "--apps", "switchback_chat"]
