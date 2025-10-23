# Stage 1: Deno builder for the frontend
# Uses Deno + esbuild for fast, dependency-free bundling
FROM docker.io/denoland/deno:2.1.4 AS deno-builder

WORKDIR /build

# Copy Switchback source
COPY src ./src
COPY tsconfig.json ./

# Copy demo app file
COPY examples/demos/csharp/app.ts ./examples/demos/csharp/app.ts

# Build with Deno (uses esbuild from npm via Deno's npm compatibility)
# This is MUCH faster than npm install + vite build - no node_modules needed!
WORKDIR /build/examples/demos/csharp
RUN mkdir -p wwwroot/dist
RUN deno run -A npm:esbuild app.ts --bundle --outfile=wwwroot/dist/app.js --format=esm --platform=browser

# C# server builder stage
FROM mcr.microsoft.com/dotnet/sdk:8.0 AS dotnet-builder

WORKDIR /build

# Copy C# project files
COPY examples/demos/csharp/CSharpDemo.csproj .
COPY examples/demos/csharp/Program.cs .

# Restore dependencies
RUN dotnet restore

# Build the application
RUN dotnet publish -c Release -o /app/publish

# Runtime stage
FROM mcr.microsoft.com/dotnet/aspnet:8.0

WORKDIR /app

# Copy published app
COPY --from=dotnet-builder /app/publish .

# Copy built JS bundle to wwwroot
RUN mkdir -p wwwroot/dist
COPY --from=deno-builder /build/examples/demos/csharp/wwwroot/dist/app.js ./wwwroot/dist/app.js

# Expose port
EXPOSE 8000

# Run the application
ENTRYPOINT ["dotnet", "CSharpDemo.dll"]
