# Multi-stage Dockerfile for OCaml + Switchback

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
COPY examples/demos/ocaml/package.json examples/demos/ocaml/vite.config.ts examples/demos/ocaml/.npmrc ./
COPY examples/demos/ocaml/app.ts ./

# Copy Switchback source
COPY src /workspace/src

# Install demo dependencies
RUN pnpm install

# Build the client bundle
RUN pnpm build

# Stage 2: Build OCaml application
FROM docker.io/ocaml/opam:alpine-ocaml-5.2 AS ocaml-builder

# Switch to opam user
USER opam
WORKDIR /home/opam

# Copy OCaml source
COPY --chown=opam:opam examples/demos/ocaml/dune-project examples/demos/ocaml/dune ./
COPY --chown=opam:opam examples/demos/ocaml/server.ml ./

# Install dependencies
RUN opam install -y dune

# Build the server
RUN eval $(opam env) && dune build server.exe

# Stage 3: Runtime
FROM alpine:latest

WORKDIR /app

# Install runtime dependencies
RUN apk add --no-cache gmp

# Copy compiled OCaml binary
COPY --from=ocaml-builder /home/opam/_build/default/server.exe /app/server

# Copy built JavaScript bundle
COPY --from=js-builder /app/dist /app/dist

EXPOSE 8000

# Start the OCaml server
CMD ["/app/server"]
