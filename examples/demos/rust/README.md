# Rust + Switchback - Image Processing Pipeline

Upload multiple images, watch them queue up, and see real-time processing updates—all without page reloads. This demo showcases **Switchback's partial reload feature**: update only specific UI sections without re-rendering the entire page.

## Features

- File upload with progress tracking
- Partial reloads (update only specific sections)
- Worker pool processing (4 parallel workers)
- Delete images from gallery
- Minimal dependencies (4 Rust crates)

## Quick Start

**Docker (recommended):**
```bash
cd examples/demos/rust
docker-compose up
```

**Local Rust:**
```bash
cd examples/demos/rust
pnpm install && pnpm build
cargo run --release --features server
```

Open http://localhost:8000

**No images?** Click "Try Sample Images" to load 15 stock photos.

## How Partial Reloads Work

**Traditional approach** - Re-render everything:
```typescript
app.reload(); // ❌ Entire page re-renders
```

**Switchback approach** - Update only what changed:
```typescript
app.reload({ only: ['processingQueue'] }); // ✅ Only queue updates
app.reload({ only: ['gallery'] }); // ✅ Only gallery updates
app.reload({ only: ['processingQueue', 'gallery'] }); // ✅ Both sections
```

**Example**: Gallery with 50 images + 10 processing jobs
- Update job progress → Only queue section re-renders
- New image completes → Only gallery re-renders
- Delete image → Only gallery re-renders

## Technical Details

### Architecture

- **Frontend**: Switchback with partial reloads
- **Backend**: Raw Rust HTTP server (no framework)
- **Processing**: 4-worker thread pool with job queue
- **Storage**: In-memory state with `Arc<Mutex>`, files in `public/uploads/`

### API

| Endpoint | Purpose |
|----------|---------|
| `POST /api/upload` | Upload image, create job |
| `GET /api/jobs?ids=...` | Get job status |
| `GET /api/gallery` | Get completed images |
| `DELETE /api/jobs/:id` | Delete job and files |
| `GET /uploads/*` | Serve processed images |

### Job Flow

```
Upload → Pending → Queued → Processing (0%→33%→66%→100%) → Complete
```

## What You'll See

1. Upload images → Individual progress bars
2. Click "Process Images" → Jobs queued
3. Watch workers process in parallel → Real-time progress (0%→33%→66%→100%)
4. Completed images appear in gallery
5. Hover to delete images

**Key observation**: Only the changing section re-renders. Queue updates don't affect gallery. Gallery additions don't affect queue.

## Why This Demo?

Demonstrates Switchback's partial reload feature: update specific UI sections without re-rendering the entire page. Useful for apps with independent updating regions (upload queue, gallery, status cards, etc.).
