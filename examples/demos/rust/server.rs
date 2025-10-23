/**
 * Rust + Switchback - Image Processing Pipeline Demo
 * Demonstrates Switchback's partial reloads and upload progress with image processing
 * Shows how Rust's image processing + Switchback's surgical UI updates = powerful UX
 */

use serde::{Deserialize, Serialize};
use std::collections::{HashMap, VecDeque};
use std::fs;
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

const PORT: u16 = 8000;
const BUFFER_SIZE: usize = 16384;
const WORKER_COUNT: usize = 4;

// Job status enum
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "lowercase")]
enum JobStatus {
    Pending,   // Uploaded but not yet queued for processing
    Queued,    // Queued for processing by workers
    Processing,
    Complete,
    Failed,
}

// Processing job structure
#[derive(Debug, Serialize, Clone)]
struct ProcessingJob {
    id: String,
    filename: String,
    status: JobStatus,
    progress: u8,
    worker_id: Option<usize>,
    created_at: u64,
    completed_at: Option<u64>,
    error: Option<String>,
    result_urls: Vec<String>,
}

// Gallery image structure
#[derive(Debug, Serialize, Clone)]
struct GalleryImage {
    id: String,
    filename: String,
    thumbnail_url: String,
    medium_url: String,
    grayscale_url: String,
    created_at: u64,
}

// Shared application state
struct AppState {
    jobs: Mutex<HashMap<String, ProcessingJob>>,
    job_queue: Mutex<VecDeque<String>>,
    gallery: Mutex<Vec<GalleryImage>>,
}

impl AppState {
    fn new() -> Self {
        // Create upload directories
        fs::create_dir_all("public/uploads/original").ok();
        fs::create_dir_all("public/uploads/thumb").ok();
        fs::create_dir_all("public/uploads/medium").ok();
        fs::create_dir_all("public/uploads/grayscale").ok();

        Self {
            jobs: Mutex::new(HashMap::new()),
            job_queue: Mutex::new(VecDeque::new()),
            gallery: Mutex::new(Vec::new()),
        }
    }

    fn create_job(&self, filename: String) -> String {
        let job_id = generate_id();
        let timestamp = timestamp_ms();

        let job = ProcessingJob {
            id: job_id.clone(),
            filename: filename.clone(),
            status: JobStatus::Pending,  // Start as pending
            progress: 0,
            worker_id: None,
            created_at: timestamp,
            completed_at: None,
            error: None,
            result_urls: Vec::new(),
        };

        {
            let mut jobs = self.jobs.lock().unwrap();
            jobs.insert(job_id.clone(), job);
        }

        // Don't add to queue yet - wait for /api/process call

        println!("Created job {} for file {} (status: pending)", job_id, filename);
        job_id
    }

    fn queue_job(&self, job_id: &str) -> bool {
        // Move job from pending to queued
        {
            let mut jobs = self.jobs.lock().unwrap();
            if let Some(job) = jobs.get_mut(job_id) {
                if job.status == JobStatus::Pending {
                    job.status = JobStatus::Queued;
                } else {
                    println!("Job {} is not in pending status", job_id);
                    return false;
                }
            } else {
                println!("Job {} not found", job_id);
                return false;
            }
        }

        {
            let mut queue = self.job_queue.lock().unwrap();
            queue.push_back(job_id.to_string());
        }

        println!("Queued job {} for processing", job_id);
        true
    }

    fn get_jobs_by_ids(&self, ids: Vec<String>) -> Vec<ProcessingJob> {
        let jobs = self.jobs.lock().unwrap();
        ids.iter()
            .filter_map(|id| jobs.get(id).cloned())
            .collect()
    }

    fn get_gallery(&self) -> Vec<GalleryImage> {
        let gallery = self.gallery.lock().unwrap();
        gallery.clone()
    }

    fn add_to_gallery(&self, image: GalleryImage) {
        let mut gallery = self.gallery.lock().unwrap();
        gallery.insert(0, image); // Add to beginning (newest first)
    }
}

fn main() {
    println!("🦀 Rust + Switchback - Image Processing Pipeline");
    println!("Starting server on http://localhost:{}", PORT);

    let state = Arc::new(AppState::new());

    // Spawn worker threads
    spawn_worker_pool(Arc::clone(&state), WORKER_COUNT);

    // Setup signal handlers for graceful shutdown
    {
        use signal_hook::consts::signal::*;
        use signal_hook::iterator::Signals;

        let mut signals = Signals::new(&[SIGINT, SIGTERM]).unwrap();
        thread::spawn(move || {
            for sig in signals.forever() {
                println!("\nReceived signal {:?}, shutting down...", sig);
                std::process::exit(0);
            }
        });
    }

    let listener = TcpListener::bind(format!("0.0.0.0:{}", PORT)).unwrap();
    println!("✅ Server running on port {}", PORT);
    println!("👷 {} workers started", WORKER_COUNT);

    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                let state = Arc::clone(&state);
                thread::spawn(move || handle_connection(stream, state));
            }
            Err(e) => eprintln!("Connection error: {}", e),
        }
    }
}

fn handle_connection(mut stream: TcpStream, state: Arc<AppState>) {
    let mut header_buffer = [0; BUFFER_SIZE];

    match stream.read(&mut header_buffer) {
        Ok(n) => {
            let (method, path, is_switchback) = parse_request(&header_buffer);
            println!("{} {} {}", method, path, if is_switchback { "(Switchback)" } else { "" });

            // For POST requests, we need to read the full body
            let full_buffer = if method == "POST" {
                if let Some(content_length) = extract_content_length(&header_buffer) {
                    // Find where headers end and body begins (search in raw bytes, not string!)
                    let double_crlf = b"\r\n\r\n";
                    let header_end = header_buffer[..n].windows(4)
                        .position(|w| w == double_crlf)
                        .map(|p| p + 4);

                    if let Some(header_end) = header_end {

                        // Allocate buffer for headers + full body
                        let total_size = header_end + content_length;
                        let mut full_buf = vec![0u8; total_size];

                        // Copy what we already read
                        full_buf[..n].copy_from_slice(&header_buffer[..n]);

                        // Read the rest of the body if needed
                        let mut total_read = n;
                        while total_read < total_size {
                            match stream.read(&mut full_buf[total_read..]) {
                                Ok(bytes_read) if bytes_read > 0 => {
                                    total_read += bytes_read;
                                }
                                _ => break,
                            }
                        }

                        println!("Read {} bytes total ({} header + {} body)", total_read, header_end, total_read - header_end);

                        // Debug: Check first few bytes of body
                        if total_read > header_end + 200 {
                            println!("First 10 bytes of body: {:?}", &full_buf[header_end..header_end + 10]);
                        }

                        full_buf
                    } else {
                        header_buffer[..n].to_vec()
                    }
                } else {
                    header_buffer[..n].to_vec()
                }
            } else {
                header_buffer[..n].to_vec()
            };

            let response = route_request(&method, &path, &full_buffer, is_switchback, &state);
            // Write response as bytes (supports binary data)
            stream.write_all(&response).ok();
            stream.flush().ok();
        }
        Err(e) => eprintln!("Error reading from stream: {}", e),
    }
}

fn parse_request(buffer: &[u8]) -> (String, String, bool) {
    let request = String::from_utf8_lossy(buffer);
    let lines: Vec<&str> = request.lines().collect();

    let (method, path) = if let Some(first_line) = lines.first() {
        let parts: Vec<&str> = first_line.split_whitespace().collect();
        (
            parts.get(0).unwrap_or(&"GET").to_string(),
            parts.get(1).unwrap_or(&"/").to_string(),
        )
    } else {
        ("GET".to_string(), "/".to_string())
    };

    let is_switchback = lines
        .iter()
        .any(|line| line.to_lowercase().contains("x-switchback"));

    (method, path, is_switchback)
}

fn route_request(
    method: &str,
    path: &str,
    buffer: &[u8],
    is_switchback: bool,
    state: &Arc<AppState>,
) -> Vec<u8> {
    match (method, path) {
        ("GET", "/") => handle_home(is_switchback, state),
        ("GET", p) if p.starts_with("/dist/") => serve_static(p),
        ("GET", p) if p.starts_with("/uploads/") => serve_static(p),
        ("GET", p) if p.starts_with("/samples/") => serve_static(p),
        ("POST", "/api/upload") => handle_upload(buffer, state),
        ("POST", "/api/process") => handle_process(buffer, state),
        ("GET", p) if p.starts_with("/api/jobs") => handle_get_jobs(p, state),
        ("GET", "/api/gallery") => handle_get_gallery(state),
        ("DELETE", p) if p.starts_with("/api/jobs/") => {
            let job_id = p.trim_start_matches("/api/jobs/");
            handle_delete_job(job_id, state)
        }
        _ => response_404(),
    }
}

fn handle_home(is_switchback: bool, state: &Arc<AppState>) -> Vec<u8> {
    if is_switchback {
        let gallery = state.get_gallery();
        let page = serde_json::json!({
            "component": "Home",
            "props": {
                "gallery": gallery,
            },
            "url": "/",
        });

        response_json(&page)
    } else {
        let html = fs::read_to_string("public/index.html").unwrap_or_else(|_| {
            format!(
                r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Image Processing Pipeline - Rust + Switchback</title>
    <script>
        window.initialPage = {{
            component: "Home",
            props: {{
                gallery: []
            }},
            url: "/"
        }};
    </script>
    <style>
        * {{ margin: 0; padding: 0; box-sizing: border-box; }}
        body {{
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            min-height: 100vh;
            color: #333;
        }}
    </style>
</head>
<body>
    <div data-swbk-app></div>
    <script type="module" src="/dist/app.js"></script>
</body>
</html>"#
            )
        });

        response_html(&html)
    }
}

fn handle_upload(buffer: &[u8], state: &Arc<AppState>) -> Vec<u8> {
    println!("=== UPLOAD START ===");
    println!("Total buffer size: {} bytes", buffer.len());

    // Debug: Check buffer content before any processing
    if buffer.len() > 600 {
        println!("Buffer bytes 574-594: {:?}", &buffer[574..594]);
    }

    // Parse multipart form data
    let boundary = extract_boundary(buffer);
    println!("Extracted boundary: {:?}", boundary);

    // Debug: Check buffer content after extract_boundary
    if buffer.len() > 600 {
        println!("Buffer bytes 574-594 after extract_boundary: {:?}", &buffer[574..594]);
    }

    if boundary.is_none() {
        println!("ERROR: No boundary found!");
        return response_json(&serde_json::json!({
            "success": false,
            "error": "No boundary found in multipart data"
        }));
    }

    let boundary = boundary.unwrap();
    println!("Using boundary: {}", boundary);

    let (filename, file_data) = match parse_multipart(buffer, &boundary) {
        Some(data) => data,
        None => {
            println!("ERROR: Failed to parse multipart!");
            return response_json(&serde_json::json!({
                "success": false,
                "error": "Failed to parse multipart data"
            }));
        }
    };

    // Generate job ID and save the file with extension
    let job_id = state.create_job(filename.clone());

    // Extract file extension from original filename
    let extension = filename.split('.').last().unwrap_or("jpg");
    let file_path = format!("public/uploads/original/{}.{}", job_id, extension);

    println!("Parsed file: {} ({} bytes)", filename, file_data.len());
    println!("First 20 bytes: {:?}", &file_data[..20.min(file_data.len())]);

    if let Err(e) = fs::write(&file_path, &file_data) {
        return response_json(&serde_json::json!({
            "success": false,
            "error": format!("Failed to save file: {}", e)
        }));
    }

    println!("Saved uploaded file to {}", file_path);
    println!("=== UPLOAD END ===");

    response_json(&serde_json::json!({
        "success": true,
        "job_id": job_id,
        "filename": filename
    }))
}

fn handle_process(buffer: &[u8], state: &Arc<AppState>) -> Vec<u8> {
    // Parse JSON body to get job IDs
    let body_start = {
        let double_crlf = b"\r\n\r\n";
        buffer.windows(4).position(|w| w == double_crlf).map(|p| p + 4)
    };

    if body_start.is_none() {
        return response_json(&serde_json::json!({
            "success": false,
            "error": "Invalid request"
        }));
    }

    let body = &buffer[body_start.unwrap()..];
    let body_str = String::from_utf8_lossy(body);

    #[derive(Deserialize)]
    struct ProcessRequest {
        job_ids: Vec<String>,
    }

    let request: ProcessRequest = match serde_json::from_str(&body_str) {
        Ok(req) => req,
        Err(e) => {
            return response_json(&serde_json::json!({
                "success": false,
                "error": format!("Invalid JSON: {}", e)
            }));
        }
    };

    println!("Processing {} jobs", request.job_ids.len());

    // Queue all jobs
    let mut queued_count = 0;
    for job_id in request.job_ids {
        if state.queue_job(&job_id) {
            queued_count += 1;
        }
    }

    response_json(&serde_json::json!({
        "success": true,
        "queued": queued_count
    }))
}

fn handle_get_jobs(path: &str, state: &Arc<AppState>) -> Vec<u8> {
    // Parse query string for job IDs
    let ids: Vec<String> = if let Some(query) = path.split('?').nth(1) {
        query
            .split('&')
            .filter_map(|param| {
                if param.starts_with("ids=") {
                    Some(param[4..].split(',').map(|s| s.to_string()).collect::<Vec<String>>())
                } else {
                    None
                }
            })
            .flatten()
            .collect()
    } else {
        Vec::new()
    };

    let jobs = if ids.is_empty() {
        // Return all jobs
        let jobs_map = state.jobs.lock().unwrap();
        jobs_map.values().cloned().collect()
    } else {
        state.get_jobs_by_ids(ids)
    };

    response_json(&jobs)
}

fn handle_get_gallery(state: &Arc<AppState>) -> Vec<u8> {
    let gallery = state.get_gallery();
    response_json(&gallery)
}

fn handle_delete_job(job_id: &str, state: &Arc<AppState>) -> Vec<u8> {
    // Remove from jobs
    {
        let mut jobs = state.jobs.lock().unwrap();
        jobs.remove(job_id);
    }

    // Remove from gallery
    {
        let mut gallery = state.gallery.lock().unwrap();
        gallery.retain(|img| img.id != job_id);
    }

    // Delete image files
    let files = vec![
        format!("public/uploads/original/{}", job_id),
        format!("public/uploads/thumb/{}.jpg", job_id),
        format!("public/uploads/medium/{}.jpg", job_id),
        format!("public/uploads/grayscale/{}.jpg", job_id),
    ];

    for file in files {
        fs::remove_file(file).ok(); // Ignore errors if file doesn't exist
    }

    response_json(&serde_json::json!({
        "success": true
    }))
}

fn serve_static(path: &str) -> Vec<u8> {
    let file_path = format!("public{}", path);

    match fs::read(&file_path) {
        Ok(contents) => {
            let content_type = if path.ends_with(".js") {
                "application/javascript"
            } else if path.ends_with(".css") {
                "text/css"
            } else if path.ends_with(".jpg") || path.ends_with(".jpeg") {
                "image/jpeg"
            } else if path.ends_with(".png") {
                "image/png"
            } else {
                "application/octet-stream"
            };

            let header = format!(
                "HTTP/1.1 200 OK\r\nContent-Type: {}\r\nContent-Length: {}\r\n\r\n",
                content_type,
                contents.len()
            );

            // Build response with binary support
            let mut response = header.into_bytes();
            response.extend_from_slice(&contents);
            response
        }
        Err(_) => response_404(),
    }
}

// Worker pool implementation
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
                        println!("Worker {} picked up job {}", worker_id, id);
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
    use image::imageops::FilterType;

    // Update status to Processing
    {
        let mut jobs = state.jobs.lock().unwrap();
        if let Some(job) = jobs.get_mut(job_id) {
            job.status = JobStatus::Processing;
            job.worker_id = Some(worker_id);
            job.progress = 10;
        }
    }

    let filename = {
        let jobs = state.jobs.lock().unwrap();
        jobs.get(job_id).map(|j| j.filename.clone()).unwrap_or_default()
    };

    // Get file extension
    let extension = filename.split('.').last().unwrap_or("jpg");
    let input_path = format!("public/uploads/original/{}.{}", job_id, extension);

    // Load the image
    let img = match image::open(&input_path) {
        Ok(img) => img,
        Err(e) => {
            eprintln!("Worker {} failed to open image: {}", worker_id, e);
            let mut jobs = state.jobs.lock().unwrap();
            if let Some(job) = jobs.get_mut(job_id) {
                job.status = JobStatus::Failed;
                job.error = Some(format!("Failed to open image: {}", e));
            }
            return;
        }
    };

    // Generate thumbnail (200x200)
    println!("Worker {} generating thumbnail for {}", worker_id, job_id);
    let thumbnail = img.thumbnail(200, 200);
    thread::sleep(Duration::from_secs(1)); // Simulate processing time
    let thumb_path = format!("public/uploads/thumb/{}.jpg", job_id);
    if let Err(e) = thumbnail.save(&thumb_path) {
        eprintln!("Worker {} failed to save thumbnail: {}", worker_id, e);
    }

    // Update progress
    {
        let mut jobs = state.jobs.lock().unwrap();
        if let Some(job) = jobs.get_mut(job_id) {
            job.progress = 33;
        }
    }

    // Generate medium size (800x800)
    println!("Worker {} generating medium size for {}", worker_id, job_id);
    let medium = img.resize(800, 800, FilterType::Lanczos3);
    thread::sleep(Duration::from_secs(1)); // Simulate processing time
    let medium_path = format!("public/uploads/medium/{}.jpg", job_id);
    if let Err(e) = medium.save(&medium_path) {
        eprintln!("Worker {} failed to save medium: {}", worker_id, e);
    }

    // Update progress
    {
        let mut jobs = state.jobs.lock().unwrap();
        if let Some(job) = jobs.get_mut(job_id) {
            job.progress = 66;
        }
    }

    // Apply grayscale filter
    println!("Worker {} applying grayscale filter for {}", worker_id, job_id);
    let grayscale = img.grayscale();
    thread::sleep(Duration::from_secs(1)); // Simulate processing time
    let gray_path = format!("public/uploads/grayscale/{}.jpg", job_id);
    if let Err(e) = grayscale.save(&gray_path) {
        eprintln!("Worker {} failed to save grayscale: {}", worker_id, e);
    }

    // Mark as complete
    let timestamp = timestamp_ms();
    {
        let mut jobs = state.jobs.lock().unwrap();
        if let Some(job) = jobs.get_mut(job_id) {
            job.status = JobStatus::Complete;
            job.progress = 100;
            job.completed_at = Some(timestamp);
            job.result_urls = vec![
                format!("/uploads/thumb/{}.jpg", job_id),
                format!("/uploads/medium/{}.jpg", job_id),
                format!("/uploads/grayscale/{}.jpg", job_id),
            ];
        }
    }

    // Add to gallery
    let gallery_image = GalleryImage {
        id: job_id.to_string(),
        filename,
        thumbnail_url: format!("/uploads/thumb/{}.jpg", job_id),
        medium_url: format!("/uploads/medium/{}.jpg", job_id),
        grayscale_url: format!("/uploads/grayscale/{}.jpg", job_id),
        created_at: timestamp,
    };

    state.add_to_gallery(gallery_image);

    println!("Worker {} completed job {}", worker_id, job_id);
}

// Utility functions

fn generate_id() -> String {
    // Generate a unique ID using timestamp + process ID + counter
    // Format: {timestamp_ms}-{process_id}-{random_suffix}
    use std::sync::atomic::{AtomicU32, Ordering};

    static COUNTER: AtomicU32 = AtomicU32::new(0);

    let timestamp = timestamp_ms();
    let pid = std::process::id();
    let counter = COUNTER.fetch_add(1, Ordering::SeqCst);

    // Create a pseudo-random suffix using timestamp bits
    let random_suffix = (timestamp ^ (pid as u64) ^ (counter as u64)) & 0xFFFF;

    format!("{:x}-{:x}-{:04x}", timestamp, pid, random_suffix)
}

fn timestamp_ms() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as u64
}

fn extract_content_length(buffer: &[u8]) -> Option<usize> {
    // Only parse the first ~1KB as headers
    let header_size = 1024.min(buffer.len());
    let request = String::from_utf8_lossy(&buffer[..header_size]);
    for line in request.lines() {
        if line.to_lowercase().starts_with("content-length:") {
            if let Some(length_str) = line.split(':').nth(1) {
                return length_str.trim().parse().ok();
            }
        }
    }
    None
}

fn extract_boundary(buffer: &[u8]) -> Option<String> {
    // Only parse the first ~1KB as headers (don't corrupt binary data!)
    let header_size = 1024.min(buffer.len());
    let request = String::from_utf8_lossy(&buffer[..header_size]);
    for line in request.lines() {
        if line.to_lowercase().starts_with("content-type:") && line.contains("boundary=") {
            if let Some(boundary_part) = line.split("boundary=").nth(1) {
                return Some(format!("--{}", boundary_part.trim()));
            }
        }
    }
    None
}

fn parse_multipart(buffer: &[u8], boundary: &str) -> Option<(String, Vec<u8>)> {
    println!("Parsing multipart with boundary: {}", boundary);
    println!("Buffer length: {} bytes", buffer.len());

    // Debug: Check what bytes are at position 729 (should be JPEG data: 574 + 155)
    if buffer.len() > 750 {
        println!("Buffer bytes 729-739 (should be JPEG magic bytes): {:?}", &buffer[729..739]);
    }

    let boundary_bytes = boundary.as_bytes();

    // First, find where the HTTP headers end and the multipart body begins
    let http_body_start = {
        let double_crlf = b"\r\n\r\n";
        buffer.windows(4).position(|w| w == double_crlf).map(|p| p + 4)
    };

    if http_body_start.is_none() {
        println!("ERROR: Could not find HTTP body start");
        return None;
    }

    let body_start = http_body_start.unwrap();
    println!("HTTP body starts at byte {}", body_start);

    // Debug: Check what's at body_start in the original buffer
    let preview_len = 20.min(buffer.len() - body_start);
    println!("First {} bytes at body_start in original buffer: {:?}", preview_len, &buffer[body_start..body_start + preview_len]);

    // Work with just the multipart body (as raw bytes - don't corrupt binary data!)
    let multipart_body = &buffer[body_start..];

    // Find where the file data starts (after \r\n\r\n in the multipart section)
    let double_crlf = b"\r\n\r\n";
    let double_lf = b"\n\n";

    let data_start_offset = multipart_body.windows(4)
        .position(|w| w == double_crlf)
        .map(|p| p + 4)
        .or_else(|| multipart_body.windows(2).position(|w| w == double_lf).map(|p| p + 2));

    if data_start_offset.is_none() {
        println!("ERROR: Could not find multipart data start marker!");
        return None;
    }

    let data_start = data_start_offset.unwrap();
    println!("Data start offset in multipart body: {}", data_start);

    // Debug: show what's at the data start position
    let preview_len = 20.min(multipart_body.len() - data_start);
    println!("Bytes at data_start position: {:?}", &multipart_body[data_start..data_start + preview_len]);

    // Now extract filename from the headers section (before data_start) ONLY
    // This way we don't corrupt the binary file data with UTF-8 conversion
    let headers_section = &multipart_body[..data_start];
    let headers_str = String::from_utf8_lossy(headers_section);

    let filename = if let Some(content_disp) = headers_str.lines().find(|l| l.contains("Content-Disposition")) {
        if let Some(filename_part) = content_disp.split("filename=\"").nth(1) {
            filename_part.split('"').next().unwrap_or("unknown.jpg").to_string()
        } else {
            "unknown.jpg".to_string()
        }
    } else {
        println!("ERROR: No Content-Disposition header found");
        return None;
    };

    println!("Found filename: {}", filename);

    // Find the closing boundary in the multipart body
    let closing_boundary = format!("\r\n{}", boundary).into_bytes();
    let alt_closing = format!("\n{}", boundary).into_bytes();

    println!("Looking for closing boundary starting at offset {}", data_start);

    let data_end_offset = multipart_body[data_start..]
        .windows(closing_boundary.len())
        .position(|w| w == &closing_boundary[..])
        .or_else(|| {
            multipart_body[data_start..]
                .windows(alt_closing.len())
                .position(|w| w == &alt_closing[..])
        });

    println!("Data end offset: {:?}", data_end_offset);

    if let Some(end_offset) = data_end_offset {
        let file_data = multipart_body[data_start..data_start + end_offset].to_vec();
        println!("Extracted {} bytes of file data", file_data.len());
        println!("First 4 bytes: {:?}", &file_data[..4.min(file_data.len())]);
        return Some((filename, file_data));
    } else {
        println!("No closing boundary found, using rest of buffer");
        // No closing boundary found, take rest of multipart body minus some safety margin
        let mut end_pos = multipart_body.len();
        // Try to find any boundary-like pattern at the end
        for i in (data_start..multipart_body.len()).rev() {
            if i + boundary_bytes.len() <= multipart_body.len()
                && &multipart_body[i..i + boundary_bytes.len()] == boundary_bytes {
                end_pos = i;
                println!("Found boundary at position {}", i);
                break;
            }
        }
        let file_data = multipart_body[data_start..end_pos].to_vec();
        println!("Extracted {} bytes of file data (no closing boundary)", file_data.len());
        return Some((filename, file_data));
    }
}

fn response_html(html: &str) -> Vec<u8> {
    let response = format!(
        "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=utf-8\r\nContent-Length: {}\r\n\r\n{}",
        html.len(),
        html
    );
    response.into_bytes()
}

fn response_json<T: Serialize>(data: &T) -> Vec<u8> {
    let json = serde_json::to_string(data).unwrap();
    let response = format!(
        "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: {}\r\n\r\n{}",
        json.len(),
        json
    );
    response.into_bytes()
}

fn response_404() -> Vec<u8> {
    let body = "404 Not Found";
    let response = format!(
        "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\nContent-Length: {}\r\n\r\n{}",
        body.len(),
        body
    );
    response.into_bytes()
}
