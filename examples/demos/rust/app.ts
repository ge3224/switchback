import { newSwitchback } from '../../../src/index.ts';

console.log('🖼️ Image Processing Pipeline app starting...');

// Simple JSX-like helper
function h(tag: string, props: any = {}, ...children: any[]) {
  const element = document.createElement(tag);

  Object.keys(props || {}).forEach(key => {
    if (key.startsWith('on')) {
      element.addEventListener(key.slice(2).toLowerCase(), props[key]);
    } else if (key === 'class' || key === 'className') {
      element.className = props[key];
    } else if (key === 'style' && typeof props[key] === 'object') {
      Object.assign(element.style, props[key]);
    } else {
      element.setAttribute(key, props[key]);
    }
  });

  children.flat().filter(Boolean).forEach(child => {
    if (typeof child === 'string' || typeof child === 'number') {
      element.appendChild(document.createTextNode(String(child)));
    } else if (child instanceof Node) {
      element.appendChild(child);
    }
  });

  return element;
}

// Global state
const state = {
  // Upload tracking
  uploads: {} as Record<string, {
    file: File;
    progress: number;
    status: 'uploading' | 'queued' | 'processing' | 'complete' | 'failed';
    jobId?: string;
    error?: string;
  }>,

  // Processing jobs
  jobs: {} as Record<string, {
    id: string;
    filename: string;
    status: 'pending' | 'queued' | 'processing' | 'complete' | 'failed';
    progress: number;
    worker_id?: number;
    created_at: number;
    completed_at?: number;
    error?: string;
    result_urls: string[];
  }>,

  // UI control
  isProcessing: false,

  // Gallery
  gallery: [] as Array<{
    id: string;
    filename: string;
    thumbnail_url: string;
    medium_url: string;
    grayscale_url: string;
    created_at: number;
  }>,

  // UI state
  selectedImage: null as any,
  pollInterval: null as number | null,
};

// Inject styles
const style = document.createElement('style');
style.textContent = `
  * { margin: 0; padding: 0; box-sizing: border-box; }

  body {
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    min-height: 100vh;
    color: #333;
  }

  .container {
    max-width: 1200px;
    margin: 0 auto;
    padding: 20px;
  }

  .header {
    background: white;
    border-radius: 12px;
    padding: 24px;
    margin-bottom: 24px;
    box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
  }

  .header h1 {
    font-size: 28px;
    margin-bottom: 8px;
    color: #667eea;
  }

  .header p {
    color: #666;
    font-size: 14px;
  }

  .upload-section {
    background: white;
    border-radius: 12px;
    padding: 24px;
    margin-bottom: 24px;
    box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
  }

  .upload-section h2 {
    font-size: 20px;
    margin-bottom: 16px;
    color: #333;
  }

  .file-input-wrapper {
    position: relative;
    display: inline-block;
  }

  .file-input {
    position: absolute;
    opacity: 0;
    width: 0;
    height: 0;
  }

  .file-input-label {
    display: inline-block;
    padding: 12px 24px;
    background: #667eea;
    color: white;
    border-radius: 8px;
    cursor: pointer;
    font-weight: 500;
    transition: background 0.2s;
  }

  .file-input-label:hover {
    background: #5568d3;
  }

  .upload-button {
    padding: 12px 32px;
    background: #764ba2;
    color: white;
    border: none;
    border-radius: 8px;
    font-size: 16px;
    font-weight: 600;
    cursor: pointer;
    transition: background 0.2s;
    margin-left: 12px;
  }

  .upload-button:hover {
    background: #653a8a;
  }

  .upload-button:disabled {
    background: #ccc;
    cursor: not-allowed;
  }

  .upload-progress {
    margin-top: 16px;
  }

  .upload-item {
    background: #f5f5f5;
    border-radius: 8px;
    padding: 12px;
    margin-bottom: 8px;
  }

  .upload-item-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 8px;
  }

  .upload-item-name {
    font-weight: 500;
    font-size: 14px;
  }

  .upload-item-status {
    font-size: 12px;
    padding: 4px 8px;
    border-radius: 4px;
    font-weight: 500;
  }

  .status-uploading {
    background: #fff4e5;
    color: #f39c12;
  }

  .status-pending {
    background: #f5f5f5;
    color: #666;
  }

  .status-queued {
    background: #e3f2fd;
    color: #2196f3;
  }

  .status-complete {
    background: #e8f5e9;
    color: #4caf50;
  }

  .progress-bar {
    height: 6px;
    background: #e0e0e0;
    border-radius: 3px;
    overflow: hidden;
  }

  .progress-fill {
    height: 100%;
    background: #667eea;
    transition: width 0.3s ease;
  }

  .queue-section {
    background: white;
    border-radius: 12px;
    padding: 24px;
    margin-bottom: 24px;
    box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
  }

  .queue-section h2 {
    font-size: 20px;
    margin-bottom: 16px;
    color: #333;
  }

  .job-card {
    background: #f9f9f9;
    border-radius: 8px;
    padding: 16px;
    margin-bottom: 12px;
    border-left: 4px solid #667eea;
  }

  .job-card.processing {
    border-left-color: #f39c12;
    animation: pulse 2s infinite;
  }

  .job-card.complete {
    border-left-color: #4caf50;
  }

  .job-card.failed {
    border-left-color: #e74c3c;
  }

  @keyframes pulse {
    0%, 100% { opacity: 1; }
    50% { opacity: 0.8; }
  }

  .job-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 12px;
  }

  .job-title {
    font-weight: 600;
    font-size: 14px;
  }

  .job-badge {
    padding: 4px 12px;
    border-radius: 12px;
    font-size: 11px;
    font-weight: 600;
    text-transform: uppercase;
  }

  .badge-pending {
    background: #f5f5f5;
    color: #666;
  }

  .badge-queued {
    background: #e3f2fd;
    color: #2196f3;
  }

  .badge-processing {
    background: #fff4e5;
    color: #f39c12;
  }

  .badge-complete {
    background: #e8f5e9;
    color: #4caf50;
  }

  .badge-failed {
    background: #ffebee;
    color: #e74c3c;
  }

  .job-meta {
    font-size: 12px;
    color: #666;
    margin-bottom: 8px;
  }

  .gallery-section {
    background: white;
    border-radius: 12px;
    padding: 24px;
    box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
  }

  .gallery-section h2 {
    font-size: 20px;
    margin-bottom: 16px;
    color: #333;
  }

  .gallery-grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(150px, 1fr));
    gap: 12px;
  }

  .gallery-item {
    position: relative;
    border-radius: 8px;
    overflow: hidden;
    cursor: pointer;
    transition: transform 0.2s, box-shadow 0.2s;
    background: #f5f5f5;
  }

  .gallery-item:hover {
    transform: translateY(-4px);
    box-shadow: 0 8px 16px rgba(0, 0, 0, 0.2);
  }

  .gallery-item img {
    width: 100%;
    height: 150px;
    object-fit: contain;
    display: block;
    background: #f5f5f5;
  }

  .gallery-item-info {
    padding: 8px;
    font-size: 12px;
    color: #666;
  }

  .gallery-item-delete {
    position: absolute;
    top: 8px;
    right: 8px;
    background: rgba(231, 76, 60, 0.9);
    color: white;
    border: none;
    border-radius: 50%;
    width: 32px;
    height: 32px;
    font-size: 18px;
    cursor: pointer;
    display: flex;
    align-items: center;
    justify-content: center;
    opacity: 0;
    transition: opacity 0.2s, background 0.2s;
    z-index: 10;
  }

  .gallery-item:hover .gallery-item-delete {
    opacity: 1;
  }

  .gallery-item-delete:hover {
    background: rgba(192, 57, 43, 1);
  }

  .empty-state {
    text-align: center;
    padding: 48px 24px;
    color: #999;
  }

  .empty-state svg {
    width: 64px;
    height: 64px;
    margin-bottom: 16px;
    opacity: 0.5;
  }

  .modal {
    position: fixed;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background: rgba(0, 0, 0, 0.8);
    display: flex;
    align-items: center;
    justify-content: center;
    z-index: 1000;
    padding: 20px;
  }

  .modal-content {
    background: white;
    border-radius: 12px;
    max-width: 90%;
    max-height: 90%;
    overflow: auto;
    padding: 24px;
  }

  .modal-close {
    position: absolute;
    top: 16px;
    right: 16px;
    background: rgba(0, 0, 0, 0.5);
    color: white;
    border: none;
    border-radius: 50%;
    width: 32px;
    height: 32px;
    font-size: 20px;
    cursor: pointer;
    transition: background 0.2s;
  }

  .modal-close:hover {
    background: rgba(0, 0, 0, 0.7);
  }

  .modal-images {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
    gap: 16px;
    margin-top: 16px;
  }

  .modal-image {
    border-radius: 8px;
    overflow: hidden;
  }

  .modal-image img {
    width: 100%;
    height: auto;
    display: block;
  }

  .modal-image-label {
    padding: 8px;
    background: #f5f5f5;
    text-align: center;
    font-size: 12px;
    font-weight: 600;
    color: #666;
  }

  .selected-files {
    margin-top: 12px;
    font-size: 14px;
    color: #666;
  }

  .footer {
    background: white;
    border-radius: 12px;
    padding: 16px 24px;
    margin-top: 24px;
    box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
    text-align: center;
    font-size: 12px;
    color: #666;
  }

  .footer a {
    color: #667eea;
    text-decoration: none;
  }

  .footer a:hover {
    text-decoration: underline;
  }
`;
document.head.appendChild(style);

// Load sample images
async function loadSampleImages() {
  const sampleFiles = [
    '/samples/sample1.jpg',
    '/samples/sample2.jpg',
    '/samples/sample3.jpg',
    '/samples/sample4.jpg',
    '/samples/sample5.jpg',
    '/samples/sample6.jpg',
    '/samples/sample7.jpg',
    '/samples/sample8.jpg',
    '/samples/sample9.jpg',
    '/samples/sample10.jpg',
    '/samples/sample11.jpg',
    '/samples/sample12.jpg',
    '/samples/sample13.jpg',
    '/samples/sample14.jpg',
    '/samples/sample15.jpg',
  ];

  // Fetch and convert to File objects
  const files: File[] = [];
  for (const url of sampleFiles) {
    try {
      const response = await fetch(url);
      const blob = await response.blob();
      const filename = url.split('/').pop() || 'sample.jpg';
      const file = new File([blob], filename, { type: 'image/jpeg' });
      files.push(file);
    } catch (error) {
      console.error(`Failed to load ${url}:`, error);
    }
  }

  if (files.length === 0) {
    alert('Failed to load sample images');
    return;
  }

  // Upload the sample files
  await uploadFiles(files);
}

// File input change handler - auto-upload on selection
async function handleFileSelect() {
  const fileInput = document.getElementById('file-input') as HTMLInputElement;
  const files = fileInput?.files;

  if (!files || files.length === 0) {
    return;
  }

  await uploadFiles(Array.from(files));

  // Clear file input so the same files can be selected again
  if (fileInput) fileInput.value = '';
}

// Common upload logic
async function uploadFiles(files: File[]) {
  // Initialize upload tracking for each file
  for (let i = 0; i < files.length; i++) {
    const file = files[i];
    state.uploads[file.name] = {
      file,
      progress: 0,
      status: 'uploading',
    };
  }

  app.reload({ only: ['uploadProgress'] });

  // Upload files sequentially (could be parallel, but sequential is easier to follow)
  for (let i = 0; i < files.length; i++) {
    const file = files[i];
    await uploadSingleFile(file);
  }

  // Scroll to processing queue section after uploads complete
  setTimeout(() => {
    const queueSection = document.querySelector('.queue-section');
    if (queueSection) {
      queueSection.scrollIntoView({ behavior: 'smooth', block: 'start' });
    }
  }, 300);

  // Don't start polling - user must click "Process Images" first
}

async function uploadSingleFile(file: File) {
  const formData = new FormData();
  formData.append('image', file);

  return new Promise((resolve, reject) => {
    const xhr = new XMLHttpRequest();

    // Track upload progress
    xhr.upload.onprogress = (e) => {
      if (e.lengthComputable) {
        state.uploads[file.name].progress = Math.round((e.loaded / e.total) * 100);
        app.reload({ only: ['uploadProgress'] });
      }
    };

    xhr.onload = () => {
      if (xhr.status === 200) {
        const result = JSON.parse(xhr.responseText);
        if (result.success) {
          state.uploads[file.name].status = 'complete';
          state.uploads[file.name].jobId = result.job_id;

          // Add to jobs tracking (status: pending until "Process Images" is clicked)
          state.jobs[result.job_id] = {
            id: result.job_id,
            filename: file.name,
            status: 'pending',
            progress: 0,
            created_at: Date.now(),
            result_urls: [],
          };

          app.reload({ only: ['uploadProgress', 'processingQueue'] });
          resolve(result);
        } else {
          state.uploads[file.name].status = 'failed';
          state.uploads[file.name].error = result.error;
          app.reload({ only: ['uploadProgress'] });
          reject(new Error(result.error));
        }
      } else {
        state.uploads[file.name].status = 'failed';
        state.uploads[file.name].error = 'Upload failed';
        app.reload({ only: ['uploadProgress'] });
        reject(new Error('Upload failed'));
      }
    };

    xhr.onerror = () => {
      state.uploads[file.name].status = 'failed';
      state.uploads[file.name].error = 'Network error';
      app.reload({ only: ['uploadProgress'] });
      reject(new Error('Network error'));
    };

    xhr.open('POST', '/api/upload');
    xhr.send(formData);
  });
}

// Start processing queued images
async function processImages() {
  const pendingJobs = Object.values(state.jobs).filter(job => job.status === 'pending');

  if (pendingJobs.length === 0) {
    alert('No images in queue to process');
    return;
  }

  state.isProcessing = true;
  app.reload({ only: ['processingQueue'] });

  // Send all pending job IDs to the server to start processing
  const jobIds = pendingJobs.map(job => job.id);

  try {
    const response = await fetch('/api/process', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ job_ids: jobIds }),
    });

    const result = await response.json();

    if (result.success) {
      // Update all jobs to queued status
      jobIds.forEach(id => {
        if (state.jobs[id]) {
          state.jobs[id].status = 'queued';
        }
      });

      app.reload({ only: ['processingQueue'] });

      // Start polling for job status
      startPolling();
    } else {
      alert('Failed to start processing: ' + result.error);
      state.isProcessing = false;
      app.reload({ only: ['processingQueue'] });
    }
  } catch (error) {
    console.error('Failed to start processing:', error);
    alert('Failed to start processing');
    state.isProcessing = false;
    app.reload({ only: ['processingQueue'] });
  }
}

// Polling for job status
function startPolling() {
  if (state.pollInterval) return; // Already polling

  state.pollInterval = window.setInterval(async () => {
    const activeJobIds = Object.values(state.jobs)
      .filter(job => job.status === 'queued' || job.status === 'processing')
      .map(job => job.id);

    if (activeJobIds.length === 0) {
      // No active jobs, stop polling
      if (state.pollInterval) {
        clearInterval(state.pollInterval);
        state.pollInterval = null;
        state.isProcessing = false;
        app.reload({ only: ['processingQueue'] });
      }
      return;
    }

    try {
      const response = await fetch(`/api/jobs?ids=${activeJobIds.join(',')}`);
      const jobs = await response.json();

      let queueChanged = false;
      let galleryChanged = false;

      jobs.forEach((job: any) => {
        const prevStatus = state.jobs[job.id]?.status;

        // Update job
        state.jobs[job.id] = job;

        if (prevStatus !== job.status) {
          queueChanged = true;

          // If job completed, fetch updated gallery
          if (job.status === 'complete') {
            galleryChanged = true;
          }
        }
      });

      // Partial reload - only update changed sections
      const sections = [];
      if (queueChanged) sections.push('processingQueue');
      if (galleryChanged) {
        sections.push('gallery');
        // Fetch updated gallery
        fetchGallery();
      }

      if (sections.length > 0) {
        app.reload({ only: sections });
      }
    } catch (error) {
      console.error('Failed to poll job status:', error);
    }
  }, 2000); // Poll every 2 seconds
}

async function fetchGallery() {
  try {
    const response = await fetch('/api/gallery');
    const gallery = await response.json();
    state.gallery = gallery;
  } catch (error) {
    console.error('Failed to fetch gallery:', error);
  }
}

function openImageModal(image: any) {
  state.selectedImage = image;
  app.reload({ only: ['modal'] });
}

function closeModal() {
  state.selectedImage = null;
  app.reload({ only: ['modal'] });
}

async function deleteImage(imageId: string, event?: Event) {
  if (event) {
    event.stopPropagation(); // Prevent opening modal when clicking delete
  }

  if (!confirm('Are you sure you want to delete this image?')) {
    return;
  }

  try {
    const response = await fetch(`/api/jobs/${imageId}`, {
      method: 'DELETE',
    });

    const result = await response.json();

    if (result.success) {
      // Remove from gallery state
      state.gallery = state.gallery.filter(img => img.id !== imageId);

      // Remove from jobs state if exists
      delete state.jobs[imageId];

      // Close modal if this image was open
      if (state.selectedImage && state.selectedImage.id === imageId) {
        state.selectedImage = null;
        app.reload({ only: ['gallery', 'modal'] });
      } else {
        app.reload({ only: ['gallery'] });
      }
    } else {
      alert('Failed to delete image');
    }
  } catch (error) {
    console.error('Failed to delete image:', error);
    alert('Failed to delete image');
  }
}

// Components

function UploadSection() {
  const uploadingCount = Object.values(state.uploads).filter(u => u.status === 'uploading').length;
  const isUploading = uploadingCount > 0;

  return h('div', { class: 'upload-section', 'data-swbk-section': 'uploadSection' },
    h('h2', {}, '📤 Upload Images'),
    h('p', { style: { marginBottom: '16px', color: '#666', fontSize: '14px' } },
      'Select images to add to the queue. Click "Process Images" when ready to start processing.'
    ),
    h('div', { style: { marginBottom: '12px' } },
      h('div', { class: 'file-input-wrapper' },
        h('input', {
          type: 'file',
          id: 'file-input',
          class: 'file-input',
          multiple: true,
          accept: 'image/*',
          onChange: handleFileSelect
        }),
        (() => {
          const label = h('label', {
            for: 'file-input',
            class: 'file-input-label'
          }, isUploading ? 'Uploading...' : '📁 Choose Images');

          // Disable pointer events when uploading
          if (isUploading) {
            label.style.opacity = '0.6';
            label.style.cursor = 'not-allowed';
          }

          return label;
        })()
      ),
      h('span', { style: { color: '#999', fontSize: '14px', marginLeft: '12px', marginRight: '12px' } }, 'OR'),
      (() => {
        const btn = h('button', {
          class: 'upload-button',
          style: { background: '#10b981', marginLeft: '0' },
          onClick: loadSampleImages
        }, isUploading ? 'Loading...' : '🎨 Try Sample Images');

        if (isUploading) {
          btn.setAttribute('disabled', 'true');
        }

        return btn;
      })()
    )
  );
}

function UploadProgress() {
  const uploads = Object.entries(state.uploads);

  if (uploads.length === 0) return null;

  return h('div', { class: 'upload-progress', 'data-swbk-section': 'uploadProgress' },
    ...uploads.map(([filename, upload]) =>
      h('div', { class: 'upload-item' },
        h('div', { class: 'upload-item-header' },
          h('span', { class: 'upload-item-name' }, filename),
          h('span', {
            class: `upload-item-status status-${upload.status}`
          }, upload.status.toUpperCase())
        ),
        h('div', { class: 'progress-bar' },
          h('div', {
            class: 'progress-fill',
            style: { width: `${upload.progress}%` }
          })
        ),
        upload.error ? h('div', { style: { color: '#e74c3c', fontSize: '12px', marginTop: '4px' } },
          upload.error
        ) : null
      )
    )
  );
}

function ProcessingQueue() {
  const pendingJobs = Object.values(state.jobs).filter(job => job.status === 'pending');
  const activeJobs = Object.values(state.jobs).filter(job =>
    job.status === 'queued' || job.status === 'processing'
  );
  const allJobs = [...pendingJobs, ...activeJobs];

  return h('div', { class: 'queue-section', 'data-swbk-section': 'processingQueue' },
    h('h2', {}, '⚙️ Processing Queue'),

    // Process Images button (only show if there are pending jobs)
    pendingJobs.length > 0 ? h('div', { style: { marginBottom: '16px' } },
      (() => {
        const btn = h('button', {
          class: 'upload-button',
          style: { marginLeft: '0', background: '#10b981' },
          onClick: processImages
        }, state.isProcessing ? 'Processing...' : `⚡ Process ${pendingJobs.length} Image${pendingJobs.length !== 1 ? 's' : ''}`);

        if (state.isProcessing) {
          btn.setAttribute('disabled', 'true');
        }

        return btn;
      })()
    ) : null,

    allJobs.length === 0 ?
      h('div', { class: 'empty-state' },
        h('p', {}, 'No jobs in queue. Upload some images to get started!')
      ) :
      h('div', {},
        ...allJobs.map(job =>
          h('div', { class: `job-card ${job.status}` },
            h('div', { class: 'job-header' },
              h('span', { class: 'job-title' }, job.filename),
              h('span', { class: `job-badge badge-${job.status}` }, job.status)
            ),
            h('div', { class: 'job-meta' },
              job.status === 'pending' ? 'Waiting to start...' :
              job.worker_id !== undefined ?
                `Worker ${job.worker_id} • ${job.progress}% complete` :
                'Waiting for worker...'
            ),
            h('div', { class: 'progress-bar' },
              h('div', {
                class: 'progress-fill',
                style: { width: `${job.progress}%` }
              })
            ),
            job.error ? h('div', { style: { color: '#e74c3c', fontSize: '12px', marginTop: '8px' } },
              `Error: ${job.error}`
            ) : null
          )
        )
      )
  );
}

function Gallery(props: { gallery: any[] }) {
  return h('div', { class: 'gallery-section', 'data-swbk-section': 'gallery' },
    h('h2', {}, '🖼️ Gallery'),
    props.gallery.length === 0 ?
      h('div', { class: 'empty-state' },
        h('p', {}, 'No images yet. Upload some images to get started!')
      ) :
      h('div', { class: 'gallery-grid' },
        ...props.gallery.map(image =>
          h('div', { class: 'gallery-item', onClick: () => openImageModal(image) },
            h('button', {
              class: 'gallery-item-delete',
              onClick: (e: Event) => deleteImage(image.id, e),
              title: 'Delete image'
            }, '×'),
            h('img', { src: image.thumbnail_url, alt: image.filename }),
            h('div', { class: 'gallery-item-info' }, image.filename)
          )
        )
      )
  );
}

function ImageModal() {
  if (!state.selectedImage) return null;

  const image = state.selectedImage;

  return h('div', { class: 'modal', 'data-swbk-section': 'modal', onClick: closeModal },
    h('div', { class: 'modal-content', onClick: (e: Event) => e.stopPropagation() },
      h('button', { class: 'modal-close', onClick: closeModal }, '×'),
      h('h2', { style: { display: 'flex', alignItems: 'center', justifyContent: 'space-between' } },
        image.filename,
        h('button', {
          class: 'upload-button',
          style: { background: '#e74c3c', padding: '8px 16px', fontSize: '14px' },
          onClick: () => deleteImage(image.id)
        }, '🗑️ Delete')
      ),
      h('div', { class: 'modal-images' },
        h('div', { class: 'modal-image' },
          h('img', { src: image.thumbnail_url, alt: 'Thumbnail' }),
          h('div', { class: 'modal-image-label' }, 'Thumbnail (200x200)')
        ),
        h('div', { class: 'modal-image' },
          h('img', { src: image.medium_url, alt: 'Medium' }),
          h('div', { class: 'modal-image-label' }, 'Medium (800x800)')
        ),
        h('div', { class: 'modal-image' },
          h('img', { src: image.grayscale_url, alt: 'Grayscale' }),
          h('div', { class: 'modal-image-label' }, 'Grayscale Filter')
        )
      )
    )
  );
}

function Footer() {
  return h('div', { class: 'footer' },
    h('p', {},
      'Sample photos from ',
      h('a', { href: 'https://unsplash.com', target: '_blank', rel: 'noopener' }, 'Unsplash'),
      ' • Free to use under the ',
      h('a', { href: 'https://unsplash.com/license', target: '_blank', rel: 'noopener' }, 'Unsplash License')
    )
  );
}

function Home(props: { gallery: any[] }) {
  // Initialize gallery state
  if (props.gallery) {
    state.gallery = props.gallery;
  }

  return h('div', { class: 'container' },
    h('div', { class: 'header' },
      h('h1', {}, '🦀 Rust + Switchback: Image Processing Pipeline'),
      h('p', {}, 'Upload multiple images and watch them process in real-time with partial UI updates!')
    ),

    UploadSection(),
    UploadProgress(),
    ProcessingQueue(),
    Gallery({ gallery: state.gallery }),
    Footer(),
    ImageModal()
  );
}

// Page mapping
const pages = {
  'Home': Home,
};

// Initialize Switchback
const app = newSwitchback({
  resolve: (name: string) => {
    return pages[name as keyof typeof pages];
  },
  setup: ({ el, App, props }) => {
    el.innerHTML = '';
    el.appendChild(App(props));
  },
  initialPage: (window as any).initialPage,
});

// Expose state for debugging
(window as any).debugState = state;

console.log('✅ Image Processing Pipeline app ready!');
console.log('Initial upload state:', state.uploads);
