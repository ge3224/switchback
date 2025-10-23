# Multi-stage Dockerfile for OCaml + Switchback

# Stage 1: Deno builder for the frontend
# Uses Deno + esbuild for fast, dependency-free bundling
FROM docker.io/denoland/deno:2.1.4 AS deno-builder

WORKDIR /build

# Copy Switchback source
COPY src ./src
COPY tsconfig.json ./

# Copy demo app file
COPY examples/demos/ocaml/app.ts ./examples/demos/ocaml/app.ts

# Build with Deno (uses esbuild from npm via Deno's npm compatibility)
# This is MUCH faster than npm install + vite build - no node_modules needed!
WORKDIR /build/examples/demos/ocaml
RUN mkdir -p dist
RUN deno run -A npm:esbuild app.ts --bundle --outfile=dist/app.js --format=esm --platform=browser

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
COPY --from=deno-builder /build/examples/demos/ocaml/dist /app/dist

EXPOSE 8000

# Start the OCaml server
CMD ["/app/server"]
