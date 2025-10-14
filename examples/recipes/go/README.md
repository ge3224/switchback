# Go Recipe - Switchback Integration

A powerful example showing how to integrate Switchback with Go's **true concurrency** model. Demonstrates a **concurrent worker pool** with goroutines processing CPU-intensive tasks in parallel - something **impossible with JavaScript's single-threaded model**!

## What's Included

- **server.go** - Go HTTP server with concurrent prime factorization worker pool
- **app.ts** - Client-side Switchback app with real-time worker visualization
- **vite.config.ts** - Bundles app.ts + Switchback into single JS file
- **Docker setup** - Multi-stage build with Go compiler

## What is True Concurrency?

True concurrency means running multiple CPU-intensive tasks **simultaneously** on multiple CPU cores. This is fundamentally different from JavaScript's async/await model.

### JavaScript (Single-Threaded):
- **Event Loop**: Non-blocking I/O, but only ONE CPU-bound task runs at a time
- **Web Workers**: Possible but heavyweight, limited communication, no shared memory
- **async/await**: Great for I/O, but tasks still execute sequentially on single thread
- **Use Case**: I/O-bound operations (network, file system)

### Go (Multi-Threaded):
- **Goroutines**: Lightweight threads (thousands can run concurrently)
- **Channels**: Type-safe communication between goroutines
- **True Parallelism**: Multiple CPU cores processing simultaneously
- **Use Case**: CPU-bound operations (computation, encoding, analysis)

### Real-World Example:

**JavaScript Backend**: Processing 4 large numbers for prime factorization
```
Number 1: [██████████] 2.3s
Number 2: [██████████] 2.1s   ← Waits for #1
Number 3: [██████████] 2.4s   ← Waits for #2
Number 4: [██████████] 2.2s   ← Waits for #3
Total: ~9 seconds
```

**Go Backend**: Same 4 numbers with 4 goroutines
```
Number 1: [██████████] 2.3s ⎤
Number 2: [██████████] 2.1s ⎥ ← All run simultaneously!
Number 3: [██████████] 2.4s ⎥
Number 4: [██████████] 2.2s ⎦
Total: ~2.4 seconds (4x faster!)
```

## How This Recipe Works

1. **4 Worker Goroutines**: Continuously listen on a job channel
2. **Job Channel**: Buffered channel (queue) holds up to 100 pending jobs
3. **Prime Factorization**: CPU-intensive task that benefits from parallelism
4. **Real-Time UI**: Client polls every second to show live worker status
5. **Interactive Demo**: Submit multiple numbers and watch them process in parallel

## Running Locally

### Prerequisites

- Go 1.21+
- Node.js 20+ with pnpm
- Or just Docker!

### Option 1: Native Build

Build the client app:

```bash
cd examples/recipes/go

# Install dependencies (uses parent's node_modules for vite/typescript)
pnpm install --dir ../../../

# Build app.ts + Switchback into dist/app.js
pnpm build
```

Build and run the Go server:

```bash
# Build Go server
go build -o server server.go

# Run server
./server
```

Open http://localhost:8000

**For development with auto-rebuild:**

```bash
# Terminal 1: Watch and rebuild client on changes
pnpm dev

# Terminal 2: Run Go server with auto-restart (install air: go install github.com/cosmtrek/air@latest)
air
# Or manually: go run server.go
```

### Option 2: Docker (Recommended)

Docker will automatically build everything:

```bash
# From examples/recipes/go directory
docker-compose up

# Or build first, then run
docker-compose build
docker-compose up
```

Open http://localhost:8000

**Note:** The Dockerfile uses a multi-stage build:
1. **JS builder stage**: Bundles app.ts with Switchback source into single JS file
2. **Go builder stage**: Compiles server.go into static binary
3. **Runtime stage**: Minimal Alpine image with both artifacts

## Try the Concurrent Processing

1. Navigate to the **Factorize** page
2. Submit a large number (e.g., 987654321)
3. Watch it get picked up by a worker immediately
4. Submit 3 more numbers quickly
5. **Watch all 4 workers processing simultaneously!**
6. See the real-time status updates showing which worker is processing which job

The magic: All 4 numbers are being factorized **at the same time** on different CPU cores!

## API Endpoints

The Go server provides these API endpoints for the worker pool:

### `POST /api/jobs`
Submit a new factorization job

```bash
curl -X POST http://localhost:8000/api/jobs \
  -H "Content-Type: application/json" \
  -d '{"number":"123456789"}'
```

Response:
```json
{
  "job": {
    "id": "job-1",
    "number": "123456789",
    "status": "pending",
    "workerId": 0,
    "factors": null,
    "startTime": "0001-01-01T00:00:00Z",
    "duration": 0
  }
}
```

### `GET /api/status`
Get real-time status of all workers and jobs

```bash
curl http://localhost:8000/api/status
```

Response:
```json
{
  "workers": [
    {
      "id": 1,
      "status": "working",
      "currentJob": "job-1"
    },
    ...
  ],
  "jobs": [
    {
      "id": "job-1",
      "number": "123456789",
      "status": "processing",
      "workerId": 1,
      "factors": null,
      "startTime": "2024-01-15T10:30:00Z",
      "duration": 0
    }
  ]
}
```

### `DELETE /api/jobs/:id`
Clear a completed job

```bash
curl -X DELETE http://localhost:8000/api/jobs/job-1
```

## Key Features Demonstrated

- ✅ **True parallel processing** - Multiple CPU cores working simultaneously
- ✅ 4 concurrent worker goroutines
- ✅ Buffered channel job queue (100 capacity)
- ✅ CPU-intensive prime factorization
- ✅ Real-time worker status updates
- ✅ Thread-safe job management with mutexes
- ✅ Graceful shutdown handling
- ✅ Interactive Switchback UI with live updates
- ✅ Cyberpunk blue theme

## File Structure

```
go/
├── server.go          # Backend with goroutine worker pool
├── app.ts             # Frontend Switchback app with live updates
├── vite.config.ts     # Vite bundler config
├── package.json       # Build scripts
├── Dockerfile         # Docker image
├── docker-compose.yml # Docker setup
└── README.md          # This file
```

## Understanding the Concurrency Model

### Goroutines

```go
// Start 4 worker goroutines
for i := 1; i <= 4; i++ {
    go worker(i, &wg)
}
```

Each goroutine runs **independently** and **concurrently**. They're multiplexed onto OS threads by Go's runtime scheduler.

### Channels

```go
jobQueue := make(chan *Job, 100)

// Producer: Add job to channel
jobQueue <- job

// Consumer: Worker receives from channel
for job := range jobQueue {
    // Process job
}
```

Channels provide **type-safe**, **synchronized** communication between goroutines without manual locking.

### Why This Beats JavaScript

JavaScript can't do this because:
1. **Single-threaded**: Only one call stack, one execution context
2. **No shared memory**: Web Workers use message passing (slow for large data)
3. **Event loop limitation**: CPU-bound tasks block the entire event loop

Go's concurrency is:
1. **Multi-threaded**: Multiple execution contexts on multiple cores
2. **Shared memory**: Goroutines can safely share data with mutexes
3. **Parallel execution**: CPU-bound tasks run truly in parallel

## Extending This Example

### Add More Workers

In `server.go`, change the worker pool size:

```go
workers = make([]*WorkerStatus, 8) // 8 concurrent workers instead of 4
```

### Add Different Job Types

Create new job types for different CPU-intensive tasks:

```go
type Job struct {
    ID     string
    Type   string // "factorize", "hash", "encrypt", etc.
    Data   string
    // ...
}
```

### Add Priority Queue

Replace the simple channel with a priority queue:

```go
import "container/heap"

type PriorityQueue []*Job
// Implement heap.Interface
```

### Add WebSocket Support

Replace polling with WebSocket for real-time updates:

```go
import "github.com/gorilla/websocket"

upgrader := websocket.Upgrader{}
conn, _ := upgrader.Upgrade(w, r, nil)
```

### Adjust Worker Pool Size Dynamically

Add endpoints to scale workers up/down:

```go
func addWorker() {
    workersMutex.Lock()
    id := len(workers) + 1
    workers = append(workers, &WorkerStatus{ID: id, Status: "idle"})
    workersMutex.Unlock()

    go worker(id, &wg)
}
```

## Performance Notes

Go's goroutines are incredibly lightweight:
- ~2KB stack size (grows/shrinks as needed)
- Fast context switching
- Efficient scheduling on multiple cores
- Can run **millions** of goroutines on single machine

The prime factorization algorithm is intentionally unoptimized to make the demo more visible. For production, use optimized algorithms like Pollard's rho or quadratic sieve.

## Benchmarking

Test the parallelism yourself:

```bash
# Submit 4 jobs simultaneously
for i in 123456789 987654321 1122334455 9988776655; do
  curl -X POST http://localhost:8000/api/jobs \
    -H "Content-Type: application/json" \
    -d "{\"number\":\"$i\"}" &
done
wait

# Check status
curl http://localhost:8000/api/status | jq
```

Watch the workers process all 4 jobs **at the same time**!

## Troubleshooting

**App not loading?**
- Make sure you've run `pnpm build` in the recipe directory
- Check that `dist/app.js` exists
- Check browser console for import errors

**Compilation errors?**
- Make sure Go is installed: `go version`
- Install Go 1.21+: https://go.dev/doc/install
- Check GOPATH is set correctly

**Workers not processing?**
- Check server logs for errors
- Verify GOMAXPROCS is set correctly: `go env GOMAXPROCS`
- Try setting explicitly: `export GOMAXPROCS=4`

**Docker issues?**
- Make sure port 8000 is not in use: `lsof -i :8000` or `netstat -an | grep 8000`
- Rebuild with `docker-compose build --no-cache`
- Check Docker has enough CPU/memory allocated

**Jobs stuck in pending?**
- Check if workers are running: `curl http://localhost:8000/api/status`
- Verify goroutines started: Check server logs for "Started worker" messages
- Queue might be full: Increase buffer size in `jobQueue := make(chan *Job, 100)`

## Security Considerations

This is a **demo application** and should not be used in production without additional hardening:

- ⚠️ No input validation beyond basic number parsing
- ⚠️ No rate limiting (could DOS with large numbers)
- ⚠️ No authentication or authorization
- ⚠️ In-memory storage (data lost on restart)
- ⚠️ No HTTPS/TLS support
- ⚠️ Unbounded job queue (memory leak potential)

For production, add:
- Input validation and sanitization
- Request rate limiting per IP
- Maximum job queue size with rejection
- Maximum number size limits
- Job timeouts to prevent infinite processing
- Authentication and session management
- HTTPS with proper certificates
- Persistent storage (Redis, PostgreSQL, etc.)
- Monitoring and alerting
- Resource limits per job

## Why Go?

This recipe demonstrates Switchback with a systems programming language optimized for concurrency:

- **Goroutines**: Lightweight, efficient concurrency primitives
- **Channels**: Safe communication between concurrent tasks
- **Performance**: Native compiled code with minimal overhead
- **Simplicity**: Concurrency is built into the language, not bolted on
- **Scalability**: Can handle thousands of concurrent operations
- **Use Cases**: APIs, microservices, real-time systems, data processing

Perfect for building high-performance backends that need true parallelism!

## Comparison with Other Recipes

- **PHP Recipe**: Shows basic navigation with a scripting language
- **Zig Recipe**: Shows modern systems programming with form handling
- **C Recipe**: Shows low-level programming with optimistic updates
- **Go Recipe**: Shows **true concurrency** with goroutines and channels

Each demonstrates different Switchback features with different language paradigms. Go's concurrency model is unique and powerful for CPU-bound workloads.

## Learn More About Go Concurrency

- [Go Concurrency Patterns](https://go.dev/blog/pipelines) - Official Go blog
- [Effective Go - Concurrency](https://go.dev/doc/effective_go#concurrency) - Official guide
- [Go by Example: Goroutines](https://gobyexample.com/goroutines) - Interactive examples
- [Concurrency is not Parallelism](https://go.dev/blog/waza-talk) - Rob Pike's talk
- [The Go Memory Model](https://go.dev/ref/mem) - How Go handles concurrent access

Go's concurrency model is one of its best features - master it to build truly scalable systems!

## Next Steps

Try modifying the recipe to:
1. Add different types of CPU-intensive jobs (hashing, encoding, etc.)
2. Implement a priority queue for job scheduling
3. Add WebSocket support for real-time updates without polling
4. Create a dashboard showing CPU usage per worker
5. Add job cancellation functionality
6. Implement worker auto-scaling based on queue size

Have fun exploring true concurrency with Go and Switchback!
