# JS builder stage
FROM node:20-alpine AS js-builder

WORKDIR /build

# Copy root package files for dependencies
COPY package.json pnpm-lock.yaml ./

# Install pnpm and root dependencies
RUN npm install -g pnpm && pnpm install --frozen-lockfile

# Copy Switchback source
COPY src ./src
COPY tsconfig.json ./

# Copy demo files maintaining structure: examples/demos/csharp/
COPY examples/demos/csharp/app.ts ./examples/demos/csharp/app.ts
COPY examples/demos/csharp/vite.config.ts ./examples/demos/csharp/vite.config.ts
COPY examples/demos/csharp/package.json ./examples/demos/csharp/package.json
COPY examples/demos/csharp/.npmrc ./examples/demos/csharp/.npmrc

# Build bundled app (includes Switchback)
WORKDIR /build/examples/demos/csharp
RUN pnpm install --reporter=silent 2>&1 | grep -v "Ignored build scripts" || true
RUN pnpm build

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
COPY --from=js-builder /build/examples/demos/csharp/wwwroot/dist/app.js ./wwwroot/dist/app.js

# Expose port
EXPOSE 8000

# Run the application
ENTRYPOINT ["dotnet", "CSharpDemo.dll"]
