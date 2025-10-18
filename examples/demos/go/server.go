/**
 * Go Recipe - Switchback HTTP Server
 * Demonstrates TRUE CONCURRENCY with goroutines and channels
 * Features a concurrent prime factorization worker pool - impossible with JavaScript!
 */

package main

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"math/big"
	"net/http"
	"os"
	"os/signal"
	"path/filepath"
	"strings"
	"sync"
	"syscall"
	"time"
)

const PORT = 8000

// Job represents a factorization job
type Job struct {
	ID        string    `json:"id"`
	Number    string    `json:"number"`
	Status    string    `json:"status"` // "pending", "processing", "completed"
	WorkerID  int       `json:"workerId"`
	Factors   []string  `json:"factors"`
	StartTime time.Time `json:"startTime"`
	Duration  float64   `json:"duration"` // in seconds
}

// WorkerStatus represents a worker's current state
type WorkerStatus struct {
	ID         int    `json:"id"`
	Status     string `json:"status"` // "idle", "working"
	CurrentJob string `json:"currentJob"`
}

// Global job queue and worker pool
var (
	jobs         = make(map[string]*Job)
	jobsMutex    sync.RWMutex
	jobQueue     = make(chan *Job, 100)
	workers      = make([]*WorkerStatus, 4) // 4 concurrent workers
	workersMutex sync.RWMutex
	jobCounter   = 0
	totalJobs    = 0
)

// Initialize workers
func init() {
	for i := 0; i < len(workers); i++ {
		workers[i] = &WorkerStatus{
			ID:     i + 1,
			Status: "idle",
		}
	}
}

// Prime factorization using trial division (intentionally slow for demo)
func factorize(n *big.Int) []string {
	var factors []string
	num := new(big.Int).Set(n)
	two := big.NewInt(2)

	// Check for 2s
	for new(big.Int).Mod(num, two).Cmp(big.NewInt(0)) == 0 {
		factors = append(factors, "2")
		num.Div(num, two)
	}

	// Check odd numbers starting from 3
	i := big.NewInt(3)
	maxI := new(big.Int).Sqrt(num)
	one := big.NewInt(1)

	for i.Cmp(maxI) <= 0 {
		for new(big.Int).Mod(num, i).Cmp(big.NewInt(0)) == 0 {
			factors = append(factors, i.String())
			num.Div(num, i)
			maxI.Sqrt(num)
		}
		i.Add(i, big.NewInt(2))
	}

	// If num > 1, it's a prime factor
	if num.Cmp(one) > 0 {
		factors = append(factors, num.String())
	}

	if len(factors) == 0 {
		factors = append(factors, "1")
	}

	return factors
}

// Worker goroutine - this runs TRUE parallel processing!
func worker(id int, wg *sync.WaitGroup) {
	defer wg.Done()

	for job := range jobQueue {
		// Update worker status
		workersMutex.Lock()
		workers[id-1].Status = "working"
		workers[id-1].CurrentJob = job.ID
		workersMutex.Unlock()

		// Update job status
		jobsMutex.Lock()
		job.Status = "processing"
		job.WorkerID = id
		job.StartTime = time.Now()
		jobsMutex.Unlock()

		// Add artificial delay BEFORE processing to make status visible
		time.Sleep(2 * time.Second)

		// Parse and factorize (CPU-intensive work!)
		num := new(big.Int)
		num.SetString(job.Number, 10)
		factors := factorize(num)

		// Update job with results
		jobsMutex.Lock()
		job.Factors = factors
		job.Status = "completed"
		job.Duration = time.Since(job.StartTime).Seconds()
		jobsMutex.Unlock()

		log.Printf("🔢 Worker %d completed job %s: %s = %v (%.2fs)",
			id, job.ID, job.Number, factors, job.Duration)

		// Worker is now idle
		workersMutex.Lock()
		workers[id-1].Status = "idle"
		workers[id-1].CurrentJob = ""
		workersMutex.Unlock()
	}
}

// Serve static files
func serveStatic(w http.ResponseWriter, path string) {
	content, err := os.ReadFile(path)
	if err != nil {
		http.Error(w, "404 Not Found", http.StatusNotFound)
		return
	}

	contentType := "application/octet-stream"
	ext := filepath.Ext(path)
	switch ext {
	case ".js":
		contentType = "application/javascript"
	case ".css":
		contentType = "text/css"
	case ".html":
		contentType = "text/html"
	}

	w.Header().Set("Content-Type", contentType)
	w.Write(content)
}

// Handle page routes
func handlePageRoute(uri string) map[string]interface{} {
	jobsMutex.RLock()
	defer jobsMutex.RUnlock()
	workersMutex.RLock()
	defer workersMutex.RUnlock()

	switch uri {
	case "/":
		activeJobs := 0
		completedJobs := 0
		for _, job := range jobs {
			if job.Status == "completed" {
				completedJobs++
			} else {
				activeJobs++
			}
		}

		return map[string]interface{}{
			"component": "Home",
			"props": map[string]interface{}{
				"stats": map[string]interface{}{
					"workers":       len(workers),
					"activeJobs":    activeJobs,
					"completedJobs": completedJobs,
					"framework":     "Go 1.21",
				},
			},
			"url": "/",
		}

	case "/factorize":
		// Get all jobs as slice
		jobList := make([]*Job, 0, len(jobs))
		for _, job := range jobs {
			jobList = append(jobList, job)
		}

		return map[string]interface{}{
			"component": "Factorize",
			"props": map[string]interface{}{
				"workers": workers,
				"jobs":    jobList,
			},
			"url": "/factorize",
		}

	case "/about":
		return map[string]interface{}{
			"component": "About",
			"props": map[string]interface{}{
				"version": "1.0.0",
				"backend": "Go 1.21",
				"features": []string{
					"True parallel processing with goroutines",
					"Concurrent worker pool (4 goroutines)",
					"Channel-based job queue",
					"Real-time prime factorization",
					"CPU-bound work (not just async I/O)",
					"Impossible with JavaScript single-threaded model",
				},
			},
			"url": "/about",
		}

	default:
		return map[string]interface{}{
			"component": "Error",
			"props": map[string]interface{}{
				"message": "Page not found",
			},
			"url": uri,
		}
	}
}

// Handle API routes
func handleAPIRoute(w http.ResponseWriter, r *http.Request) bool {
	if !strings.HasPrefix(r.URL.Path, "/api/") {
		return false
	}

	w.Header().Set("Content-Type", "application/json")

	// POST /api/jobs - Submit new factorization job
	if r.URL.Path == "/api/jobs" && r.Method == "POST" {
		body, _ := io.ReadAll(r.Body)
		var data map[string]string
		json.Unmarshal(body, &data)

		number := data["number"]
		if number == "" {
			json.NewEncoder(w).Encode(map[string]string{"error": "Number is required"})
			return true
		}

		// Validate number
		num := new(big.Int)
		_, ok := num.SetString(number, 10)
		if !ok || num.Cmp(big.NewInt(1)) <= 0 {
			json.NewEncoder(w).Encode(map[string]string{"error": "Invalid number"})
			return true
		}

		// Create job
		jobsMutex.Lock()
		jobCounter++
		totalJobs++
		jobID := fmt.Sprintf("job-%d", jobCounter)
		job := &Job{
			ID:     jobID,
			Number: number,
			Status: "pending",
		}
		jobs[jobID] = job
		jobsMutex.Unlock()

		// Add to queue (non-blocking)
		select {
		case jobQueue <- job:
			log.Printf("📝 New job queued: %s (number: %s)", jobID, number)
		default:
			jobsMutex.Lock()
			job.Status = "failed"
			jobsMutex.Unlock()
			json.NewEncoder(w).Encode(map[string]string{"error": "Queue is full"})
			return true
		}

		json.NewEncoder(w).Encode(map[string]interface{}{
			"job": job,
		})
		return true
	}

	// GET /api/status - Get current status of all workers and jobs
	if r.URL.Path == "/api/status" && r.Method == "GET" {
		jobsMutex.RLock()
		workersMutex.RLock()

		// Create deep copies to avoid race conditions
		workersCopy := make([]WorkerStatus, len(workers))
		for i, worker := range workers {
			workersCopy[i] = WorkerStatus{
				ID:         worker.ID,
				Status:     worker.Status,
				CurrentJob: worker.CurrentJob,
			}
		}

		jobList := make([]*Job, 0, len(jobs))
		for _, job := range jobs {
			jobList = append(jobList, job)
		}

		workersMutex.RUnlock()
		jobsMutex.RUnlock()

		response := map[string]interface{}{
			"workers": workersCopy,
			"jobs":    jobList,
		}

		json.NewEncoder(w).Encode(response)
		return true
	}

	// DELETE /api/jobs/:id - Clear a job
	if strings.HasPrefix(r.URL.Path, "/api/jobs/") && r.Method == "DELETE" {
		jobID := strings.TrimPrefix(r.URL.Path, "/api/jobs/")

		jobsMutex.Lock()
		delete(jobs, jobID)
		jobsMutex.Unlock()

		json.NewEncoder(w).Encode(map[string]bool{"success": true})
		return true
	}

	http.Error(w, "Not found", http.StatusNotFound)
	return true
}

// Main HTTP handler
func handler(w http.ResponseWriter, r *http.Request) {
	// Serve static files
	if strings.HasPrefix(r.URL.Path, "/dist/") {
		serveStatic(w, r.URL.Path[1:])
		return
	}

	// Handle API routes
	if handleAPIRoute(w, r) {
		return
	}

	// Handle page routes
	pageData := handlePageRoute(r.URL.Path)
	jsonBytes, _ := json.Marshal(pageData)

	// Check if this is a Switchback request
	isSwitchback := r.Header.Get("X-Switchback") != ""

	if isSwitchback {
		w.Header().Set("Content-Type", "application/json")
		w.Write(jsonBytes)
	} else {
		w.Header().Set("Content-Type", "text/html")
		html := fmt.Sprintf(`<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Go Recipe - Switchback</title>
    <script>window.initialPage = %s;</script>
</head>
<body>
    <div data-swbk-app>
        <div style="padding: 2rem; text-align: center; background: #1a1a2e; color: #16f4d0;">
            <p>Loading Switchback app...</p>
        </div>
    </div>
    <script type="module" src="/dist/app.js"></script>
</body>
</html>`, jsonBytes)
		w.Write([]byte(html))
	}
}

func main() {
	// Start worker pool
	var wg sync.WaitGroup
	for i := 1; i <= len(workers); i++ {
		wg.Add(1)
		go worker(i, &wg)
		log.Printf("🚀 Started worker %d", i)
	}

	// Setup HTTP server
	http.HandleFunc("/", handler)

	addr := fmt.Sprintf(":%d", PORT)
	server := &http.Server{
		Addr:    addr,
		Handler: nil,
	}

	log.Printf("🔵 Go server listening on http://0.0.0.0:%d", PORT)
	log.Printf("   Try concurrent factorization at /factorize!")
	log.Printf("   %d worker goroutines ready for TRUE parallel processing", len(workers))

	// Setup graceful shutdown
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)

	// Start server in goroutine
	go func() {
		if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			log.Fatalf("Server error: %v", err)
		}
	}()

	// Wait for shutdown signal
	<-sigChan
	log.Println("\n🔵 Shutting down gracefully...")

	// Close job queue to stop workers
	close(jobQueue)

	// Shutdown HTTP server with timeout
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	if err := server.Shutdown(ctx); err != nil {
		log.Printf("Server shutdown error: %v", err)
	}

	// Wait for workers to finish
	wg.Wait()

	log.Println("🔵 Server stopped")
}
