# Rust + Switchback - Image Processing Pipeline

A demo showcasing **Switchback's partial reloads and upload progress** with Rust's powerful image processing capabilities. Upload multiple images, watch them queue up, and see real-time processing updates without any page reloads!

## 🎯 What This Demonstrates

This demo highlights **Switchback's underutilized features** with a practical use case:

### Switchback Features Highlighted:
- ✅ **File upload with progress tracking** - Watch upload progress bars fill up
- ✅ **Partial reloads** - Update only specific UI sections (queue, gallery) without re-rendering the page
- ✅ **Multiple simultaneous uploads** - Queue multiple images and track each independently
- ✅ **Real-time status updates** - Poll for processing status with surgical UI updates
- ✅ **Error handling** - Graceful fallbacks for failed uploads or processing errors
- ✅ **Zero page reloads** - Entire workflow happens without leaving the page

### Why This Matters:
Traditional SPAs either:
- Use heavyweight frameworks (React, Vue) for partial updates, OR
- Reload the entire page state on every change

**Switchback gives you surgical updates with vanilla JavaScript.** Update just the processing queue card, or just add one image to the gallery - nothing else re-renders!

## 🏗️ Architecture

```
┌─────────────────────────────────────────────────┐
│  Switchback (Frontend)                          │
│  ├─ Multi-file upload with per-file progress    │
│  ├─ Partial reload: only update queue cards     │
│  ├─ Partial reload: only update gallery         │
│  ├─ Polling for status (smart partial updates)  │
│  └─ Form interception for seamless uploads      │
└─────────────────┬───────────────────────────────┘
                  │ HTTP + Multipart Form Data
┌─────────────────▼───────────────────────────────┐
│  Rust Backend (No framework!)                    │
│  ├─ Raw TCP HTTP server                         │
│  ├─ Multipart form parser                       │
│  ├─ Worker thread pool (4 workers)              │
│  ├─ Job queue (FIFO processing)                 │
│  └─ In-memory state with Arc<Mutex>             │
└─────────────────┬───────────────────────────────┘
                  │
┌─────────────────▼───────────────────────────────┐
│  Image Processing Pipeline                       │
│  ├─ Generate thumbnail (200x200)                │
│  ├─ Create medium size (800x800)                │
│  ├─ Apply filters (grayscale, blur, sharpen)    │
│  ├─ Save to public/uploads/                     │
│  └─ Update job status atomically                │
└─────────────────────────────────────────────────┘
```

## 🔗 User Flow

| Step | User Action | Switchback | Rust Backend | Worker Pool |
|------|-------------|-----------|--------------|-------------|
| 1 | Clicks "Choose Images" or "Try Sample Images" | File dialog opens (or samples load) | - | - |
| 2 | Selects 5 images | Auto-upload starts, shows 5 progress bars | - | - |
| 3 | Uploading... | Progress bars update (10%...50%...100%) | Receives multipart data, saves files | - |
| 4 | Upload complete | **Partial reload**: queue section shows pending jobs | Returns 5 job IDs (status: pending) | Jobs waiting |
| 5 | Clicks "Process Images" | Button disabled, polling starts | Queues all pending jobs | - |
| 6 | Processing starts | **Partial reload**: queue cards update | Job statuses: pending → queued | Workers pick up jobs |
| 7 | Jobs processing | **Partial reload**: update job cards only | Status: Processing (33%...66%...) | Processing images |
| 8 | Jobs complete | **Partial reload**: queue + gallery | Status: Complete | Workers idle |

**The magic**: Simple two-step UX (select → auto-queue → process) with full user control. The gallery never re-renders until a new image completes. The queue cards update independently. Zero unnecessary DOM updates!

## 🚀 Quick Start

Want to see Switchback's partial reloads without installing Rust?

```bash
cd examples/demos/rust
docker-compose up
```

Open http://localhost:8000

**Don't have images to upload?** No problem! Click the **"Try Sample Images"** button to automatically queue 5 stock photos, then click "Process Images" to see the demo in action!

## Running Natively

To run this demo with a local Rust installation:

1. Install Rust: https://rustup.rs/
2. Build the frontend:
   ```bash
   cd examples/demos/rust
   pnpm install
   pnpm build
   ```
3. Run the server:
   ```bash
   cargo run --release --features server
   ```
4. Open http://localhost:8000

## 🎓 How It Works

### 1. Multi-File Upload with Progress Tracking

```typescript
// app.ts - Upload multiple files with individual progress bars
async function uploadFiles(files: File[]) {
  // Initialize upload tracking for each file
  for (const file of files) {
    state.uploads[file.name] = { progress: 0, status: 'uploading' };
  }
  app.reload({ only: ['uploadProgress'] }); // Partial reload!

  // Upload files sequentially
  for (const file of files) {
    const formData = new FormData();
    formData.append('image', file);
    const xhr = new XMLHttpRequest();

    xhr.upload.onprogress = (e) => {
      state.uploads[file.name].progress = (e.loaded / e.total) * 100;
      app.reload({ only: ['uploadProgress'] }); // Update just this progress bar
    };

    xhr.onload = () => {
      const result = JSON.parse(xhr.responseText);
      // Job created with status: 'pending' (not yet queued for processing)
      state.jobs[result.job_id] = {
        id: result.job_id,
        filename: file.name,
        status: 'pending',
        progress: 0,
      };
      app.reload({ only: ['uploadProgress', 'processingQueue'] }); // Two sections
    };

    xhr.open('POST', '/api/upload');
    xhr.send(formData);
  }
  // Don't start processing - user must click "Process Images" button
}
```

### 2. User-Controlled Processing

```typescript
// app.ts - User clicks "Process Images" button
async function processImages() {
  const pendingJobs = Object.values(state.jobs).filter(job => job.status === 'pending');
  const jobIds = pendingJobs.map(job => job.id);

  // Send all pending job IDs to server to start processing
  const response = await fetch('/api/process', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ job_ids: jobIds }),
  });

  if (response.ok) {
    // Update all jobs to queued status
    jobIds.forEach(id => {
      state.jobs[id].status = 'queued';
    });
    app.reload({ only: ['processingQueue'] }); // Partial reload!

    // Start polling for job status
    startPolling();
  }
}
```

**Why two-step UX?**
- Users can add multiple batches to the queue (just keep selecting more images!)
- Review what's queued before triggering expensive processing
- Better control over when CPU-intensive operations happen
- Can mix uploaded images and sample images in the same queue

### 3. Partial Reloads for Surgical UI Updates

```typescript
// Traditional approach: Re-render everything
app.reload(); // ❌ Entire page re-renders

// Switchback approach: Update only what changed
app.reload({ only: ['processingQueue'] }); // ✅ Only queue cards update

// Update multiple sections
app.reload({ only: ['processingQueue', 'gallery'] }); // ✅ Queue + Gallery
```

**Why this matters:**
- Gallery has 50 images → doesn't re-render when queue updates
- Queue has 10 jobs → doesn't re-render when one completes
- Upload progress bars → each updates independently
- **Result**: Blazingly fast UI, no flickering, no wasted CPU

### 4. Smart Polling with Partial Updates

```typescript
// Poll for job status updates
async function pollJobStatus() {
  const jobIds = Object.values(state.uploads)
    .filter(u => u.status === 'queued' || u.status === 'processing')
    .map(u => u.jobId);

  if (jobIds.length === 0) return; // No active jobs

  const response = await fetch(`/api/jobs?ids=${jobIds.join(',')}`);
  const jobs = await response.json();

  let queueChanged = false;
  let galleryChanged = false;

  jobs.forEach(job => {
    const upload = Object.values(state.uploads).find(u => u.jobId === job.id);
    if (upload.status !== job.status) {
      upload.status = job.status;
      upload.progress = job.progress;
      queueChanged = true;

      if (job.status === 'complete') {
        state.gallery.unshift(job.result); // Add to gallery
        galleryChanged = true;
      }
    }
  });

  // Only reload what changed!
  const sections = [];
  if (queueChanged) sections.push('processingQueue');
  if (galleryChanged) sections.push('gallery');
  if (sections.length > 0) app.reload({ only: sections });
}

// Poll every 2 seconds
setInterval(pollJobStatus, 2000);
```

### 5. Rust Backend - Worker Pool

```rust
// server.rs - Worker thread pool
fn spawn_worker_pool(state: Arc<AppState>, worker_count: usize) {
    for worker_id in 0..worker_count {
        let state = Arc::clone(&state);
        thread::spawn(move || {
            println!("Worker {} started", worker_id);

            loop {
                // Try to get a job from the queue
                let job_id = {
                    let mut queue = state.job_queue.lock().unwrap();
                    queue.pop_front()
                };

                match job_id {
                    Some(id) => {
                        println!("Worker {} processing job {}", worker_id, id);
                        process_image(&state, &id, worker_id);
                    }
                    None => {
                        // No jobs, sleep and try again
                        thread::sleep(Duration::from_millis(500));
                    }
                }
            }
        });
    }
}

fn process_image(state: &Arc<AppState>, job_id: &str, worker_id: usize) {
    // Update status to Processing
    {
        let mut jobs = state.jobs.lock().unwrap();
        if let Some(job) = jobs.get_mut(job_id) {
            job.status = JobStatus::Processing;
            job.worker_id = Some(worker_id);
        }
    }

    // Load the uploaded image
    let input_path = format!("public/uploads/original/{}", job_id);
    let img = match image::open(&input_path) {
        Ok(img) => img,
        Err(e) => {
            // Mark job as failed
            let mut jobs = state.jobs.lock().unwrap();
            if let Some(job) = jobs.get_mut(job_id) {
                job.status = JobStatus::Failed;
                job.error = Some(format!("Failed to open image: {}", e));
            }
            return;
        }
    };

    // Generate thumbnail (simulates work with sleep)
    let thumbnail = img.thumbnail(200, 200);
    thread::sleep(Duration::from_secs(1)); // Simulate processing time
    thumbnail.save(format!("public/uploads/thumb/{}.jpg", job_id)).ok();

    // Update progress
    {
        let mut jobs = state.jobs.lock().unwrap();
        if let Some(job) = jobs.get_mut(job_id) {
            job.progress = 33;
        }
    }

    // Generate medium size
    let medium = img.thumbnail(800, 800);
    thread::sleep(Duration::from_secs(1));
    medium.save(format!("public/uploads/medium/{}.jpg", job_id)).ok();

    // Update progress
    {
        let mut jobs = state.jobs.lock().unwrap();
        if let Some(job) = jobs.get_mut(job_id) {
            job.progress = 66;
        }
    }

    // Apply grayscale filter
    let grayscale = img.grayscale();
    thread::sleep(Duration::from_secs(1));
    grayscale.save(format!("public/uploads/grayscale/{}.jpg", job_id)).ok();

    // Mark as complete
    {
        let mut jobs = state.jobs.lock().unwrap();
        if let Some(job) = jobs.get_mut(job_id) {
            job.status = JobStatus::Complete;
            job.progress = 100;
            job.completed_at = Some(timestamp_ms());
            job.result_urls = vec![
                format!("/uploads/thumb/{}.jpg", job_id),
                format!("/uploads/medium/{}.jpg", job_id),
                format!("/uploads/grayscale/{}.jpg", job_id),
            ];
        }
    }

    println!("Worker {} completed job {}", worker_id, job_id);
}
```

## 📊 What You'll See

1. **Home Page**:
   - "Choose Images" button
   - "Try Sample Images" button
   - Empty gallery (or previously processed images)
   - Empty processing queue

2. **Auto-Upload Phase** (immediately after selecting files):
   - Individual progress bars for each file
   - Percentage updates in real-time (0% → 100%)
   - **Partial reload**: Only upload progress section updates

3. **Queued Jobs** (after upload completes):
   - Jobs appear in queue with status "pending"
   - **"Process X Images" button appears** (green button)
   - Can select more images to add to the queue!
   - **Partial reload**: Only queue section updates

4. **Processing Phase** (after clicking "Process Images"):
   - Button becomes disabled ("Processing...")
   - Job cards update: pending → queued → processing
   - Card for each image showing:
     - Filename
     - Status badge (Pending → Queued → Processing → Complete)
     - Progress bar (0% → 33% → 66% → 100%)
     - Which worker is processing it
   - **Partial reload**: Only the specific job card updates

5. **Gallery** (as jobs complete):
   - Grid of processed images
   - Three versions: Thumbnail, Medium, Grayscale
   - Click to view full size
   - **Partial reload**: Only gallery section updates when new image added

6. **Real-Time Updates**:
   - Watch multiple workers process images in parallel
   - See progress bars update every second
   - Images appear in gallery as they complete
   - All without a single page reload!

## 💡 Key Takeaways

### Why Partial Reloads Are Powerful

**Scenario**: You have 50 images in your gallery and 10 jobs processing.

**Without partial reloads:**
```typescript
// Update one job status
state.jobs[0].progress = 50;
app.reload(); // ❌ Re-renders:
              // - 50 gallery images (unchanged)
              // - 10 job cards (9 unchanged)
              // - Header, footer, etc. (unchanged)
              // Result: Wasted CPU, possible flickering
```

**With partial reloads:**
```typescript
// Update one job status
state.jobs[0].progress = 50;
app.reload({ only: ['processingQueue'] }); // ✅ Re-renders:
                                            // - 10 job cards
                                            // Result: Fast, surgical update
```

### Real-World Use Cases

This pattern is perfect for:
- 📸 **Photo upload services** - Flickr, Imgur, Instagram
- 📹 **Video processing** - YouTube, TikTok, Vimeo
- 📄 **Document conversion** - PDF converters, OCR services
- 🎵 **Audio transcoding** - SoundCloud, Spotify uploads
- 📊 **Data import tools** - CSV imports, database migrations
- 🔄 **Batch operations** - Bulk email sends, report generation

### Comparison with Other Demos

| Demo | Switchback Feature | Backend Strength | Update Method |
|------|-------------------|------------------|---------------|
| **Go** | Real-time polling | Goroutines for CPU tasks | Full page data |
| **Erlang** | SSR + HTML morphing | Actor model | Server renders HTML |
| **C** | Optimistic updates | Low-level HTTP | Optimistic then confirm |
| **Rust** | **Partial reloads + upload progress** | **Image processing** | **Surgical UI updates** |

The Rust demo is the **only one** showcasing Switchback's partial reload feature - the secret weapon for building fast, efficient UIs!

## 🔧 Technical Details

### File Structure

```
rust/
├── server.rs              # Rust HTTP server + worker pool
├── app.ts                 # Switchback frontend with partial reloads
├── Cargo.toml             # Rust dependencies (image crate)
├── package.json           # Frontend build scripts
├── vite.config.ts         # Bundles app.ts + Switchback
├── Dockerfile             # Multi-stage build
├── docker-compose.yml     # Docker setup
└── public/
    └── uploads/           # Processed images stored here
        ├── original/      # Original uploads
        ├── thumb/         # 200x200 thumbnails
        ├── medium/        # 800x800 resized
        └── grayscale/     # Grayscale filter
```

### Dependencies

**Rust** (minimal):
```toml
image = "0.24"        # Image processing (resize, filters)
uuid = "1.0"          # Generate unique job IDs
serde = "1.0"         # JSON serialization
serde_json = "1.0"    # JSON parsing
```

**Frontend**:
- Switchback (vendored from `../../../src/`)
- Vite (build tool)
- TypeScript

### API Endpoints

| Method | Endpoint | Purpose | Returns |
|--------|----------|---------|---------|
| GET | `/` | Serves the Switchback app HTML | HTML |
| GET | `/dist/app.js` | Serves bundled JavaScript | JS bundle |
| POST | `/api/upload` | Upload image, create job | `{ job_id, filename }` |
| GET | `/api/jobs?ids=...` | Get status of multiple jobs | `[{ id, status, progress, ... }]` |
| GET | `/api/gallery` | Get all completed images | `[{ id, urls, ... }]` |
| DELETE | `/api/jobs/:id` | Cancel/delete a job | `{ success: true }` |
| GET | `/uploads/*` | Serve processed images | Image file |

### Job Status Flow

```
Upload → Queued → Processing (0% → 33% → 66% → 100%) → Complete
           ↓
         Failed (if error occurs)
```

### Data Structures

```rust
struct ProcessingJob {
    id: String,                  // UUID
    filename: String,            // Original filename
    status: JobStatus,           // Current status
    progress: u8,                // 0-100
    worker_id: Option<usize>,    // Which worker is processing
    created_at: u64,             // Timestamp (ms)
    completed_at: Option<u64>,   // Timestamp when done
    error: Option<String>,       // Error message if failed
    result_urls: Vec<String>,    // URLs to processed images
}

enum JobStatus {
    Queued,
    Processing,
    Complete,
    Failed,
}

struct AppState {
    jobs: Mutex<HashMap<String, ProcessingJob>>,
    job_queue: Mutex<VecDeque<String>>,
    gallery: Mutex<Vec<GalleryImage>>,
}
```

## 🎨 Customization Ideas

### 1. Add More Image Operations

```rust
// In process_image()
let sepia = apply_sepia(&img);
sepia.save(format!("public/uploads/sepia/{}.jpg", job_id)).ok();

let blur = img.blur(5.0);
blur.save(format!("public/uploads/blur/{}.jpg", job_id)).ok();

let rotated = img.rotate90();
rotated.save(format!("public/uploads/rotated/{}.jpg", job_id)).ok();
```

### 2. Add User-Selectable Filters

Let users choose which filters to apply:

```typescript
// app.ts
state.selectedFilters = {
  thumbnail: true,
  medium: true,
  grayscale: false,
  sepia: true,
  blur: false,
};

// Send filters to backend
formData.append('filters', JSON.stringify(state.selectedFilters));
```

### 3. Add Real-Time Progress via SSE

Replace polling with Server-Sent Events:

```rust
// server.rs
fn handle_sse_stream(stream: TcpStream, job_id: String) {
    send_sse_header(&stream);

    loop {
        let status = get_job_status(&job_id);
        send_sse_event(&stream, "progress", &status);

        if status.is_complete() {
            break;
        }

        thread::sleep(Duration::from_millis(500));
    }
}
```

```typescript
// app.ts
const eventSource = new EventSource(`/api/jobs/${jobId}/stream`);
eventSource.onmessage = (e) => {
  const status = JSON.parse(e.data);
  updateJobStatus(jobId, status);
  app.reload({ only: ['processingQueue'] });
};
```

### 4. Add Batch Download

Let users download all processed images as a ZIP:

```rust
// Use zip crate
fn create_zip_archive(job_ids: Vec<String>) -> Vec<u8> {
    let mut zip = ZipWriter::new(Cursor::new(Vec::new()));

    for job_id in job_ids {
        // Add each processed image to ZIP
        zip.start_file(format!("{}.jpg", job_id), FileOptions::default());
        // Write image data...
    }

    zip.finish().unwrap().into_inner()
}
```

### 5. Add Image Metadata Extraction

Show EXIF data:

```rust
// Use kamadak-exif crate
fn extract_metadata(path: &str) -> ImageMetadata {
    let file = std::fs::File::open(path)?;
    let mut bufreader = std::io::BufReader::new(file);
    let exifreader = exif::Reader::new();
    let exif = exifreader.read_from_container(&mut bufreader)?;

    ImageMetadata {
        width: exif.get_field(Tag::ImageWidth, In::PRIMARY)?,
        height: exif.get_field(Tag::ImageHeight, In::PRIMARY)?,
        camera: exif.get_field(Tag::Model, In::PRIMARY)?,
        date_taken: exif.get_field(Tag::DateTime, In::PRIMARY)?,
    }
}
```

## 🐛 Troubleshooting

**Images not uploading?**
- Check browser console for CORS errors
- Verify max upload size: `client_max_body_size` in nginx or server config
- Check file permissions on `public/uploads/` directories

**Progress bars stuck?**
- Check that polling is running: look for XHR requests every 2s
- Verify workers are running: check server logs for "Worker X processing job Y"
- Try refreshing the page

**Images not displaying?**
- Check that processed images exist in `public/uploads/`
- Verify paths are correct: `/uploads/thumb/{job_id}.jpg`
- Check browser DevTools Network tab for 404 errors

**Workers not processing?**
- Check server logs: should see "Worker X started"
- Verify job queue has jobs: `state.job_queue.len() > 0`
- Check for panics in worker threads

**Docker issues?**
- Make sure port 8000 is not in use: `lsof -i :8000`
- Rebuild with `docker-compose build --no-cache`
- Check logs: `docker-compose logs -f`

## 📚 Learn More

### Switchback Features
- [Partial Reloads Documentation](#) - How to update specific UI sections
- [Upload Progress Tracking](#) - XMLHttpRequest progress events
- [Form Interception](#) - Async form submissions

### Rust Image Processing
- [image crate](https://github.com/image-rs/image) - Image processing library
- [Image Filters](https://docs.rs/image/latest/image/imageops/index.html) - Available operations
- [Performance Tips](https://docs.rs/image/latest/image/index.html#performance) - Optimize processing

### Real-World Examples
- [Cloudinary](https://cloudinary.com/) - Cloud image processing
- [Imgix](https://imgix.com/) - Real-time image transformation
- [ImageMagick](https://imagemagick.org/) - Command-line image processing

## 🌟 Next Steps

Try these enhancements:

1. **Add more filters**: Sepia, edge detection, contrast adjustment
2. **Add image cropping**: Let users select crop area before upload
3. **Add WebSocket support**: Real-time updates without polling
4. **Add batch operations**: Process all images with one click
5. **Add download all**: ZIP archive of processed images
6. **Add image comparison**: Side-by-side before/after view
7. **Add undo/redo**: Revert to previous processing step
8. **Add sharing**: Generate shareable links to processed images

## 💬 Why This Demo Exists

Previous demos showed various Switchback features, but **partial reloads** were underutilized. This is arguably Switchback's most powerful feature - the ability to update surgical sections of your UI without re-rendering everything.

This image processing demo shows why partial reloads matter:
- **Performance**: Only update what changed
- **UX**: No flickering, no jank
- **Simplicity**: No complex state management libraries needed
- **Vanilla JS**: No React, Vue, or Angular required

**Switchback + Rust = Fast backend + Fast frontend** 🚀

Build complex, real-time UIs with vanilla JavaScript and surgical DOM updates!
