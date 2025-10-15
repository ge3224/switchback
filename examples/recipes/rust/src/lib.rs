use wasm_bindgen::prelude::*;

/// Image processor for applying filters to RGBA images
#[wasm_bindgen]
pub struct ImageProcessor {
    width: usize,
    height: usize,
    data: Vec<u8>,
}

#[wasm_bindgen]
impl ImageProcessor {
    /// Create a new image processor with RGBA data
    #[wasm_bindgen(constructor)]
    pub fn new(width: usize, height: usize, data: Vec<u8>) -> ImageProcessor {
        ImageProcessor {
            width,
            height,
            data,
        }
    }

    /// Get the processed image data
    pub fn get_data(&self) -> Vec<u8> {
        self.data.clone()
    }

    /// Apply grayscale filter
    pub fn grayscale(&mut self) {
        for i in (0..self.data.len()).step_by(4) {
            let r = self.data[i] as f32;
            let g = self.data[i + 1] as f32;
            let b = self.data[i + 2] as f32;

            // Use luminosity method for grayscale
            let gray = (0.299 * r + 0.587 * g + 0.114 * b) as u8;

            self.data[i] = gray;
            self.data[i + 1] = gray;
            self.data[i + 2] = gray;
        }
    }

    /// Apply sepia filter
    pub fn sepia(&mut self) {
        for i in (0..self.data.len()).step_by(4) {
            let r = self.data[i] as f32;
            let g = self.data[i + 1] as f32;
            let b = self.data[i + 2] as f32;

            let tr = (0.393 * r + 0.769 * g + 0.189 * b).min(255.0) as u8;
            let tg = (0.349 * r + 0.686 * g + 0.168 * b).min(255.0) as u8;
            let tb = (0.272 * r + 0.534 * g + 0.131 * b).min(255.0) as u8;

            self.data[i] = tr;
            self.data[i + 1] = tg;
            self.data[i + 2] = tb;
        }
    }

    /// Invert colors
    pub fn invert(&mut self) {
        for i in (0..self.data.len()).step_by(4) {
            self.data[i] = 255 - self.data[i];
            self.data[i + 1] = 255 - self.data[i + 1];
            self.data[i + 2] = 255 - self.data[i + 2];
        }
    }

    /// Adjust brightness (-255 to 255)
    pub fn brightness(&mut self, amount: i32) {
        for i in (0..self.data.len()).step_by(4) {
            self.data[i] = ((self.data[i] as i32 + amount).max(0).min(255)) as u8;
            self.data[i + 1] = ((self.data[i + 1] as i32 + amount).max(0).min(255)) as u8;
            self.data[i + 2] = ((self.data[i + 2] as i32 + amount).max(0).min(255)) as u8;
        }
    }

    /// Adjust contrast (0.0 to 2.0, where 1.0 is normal)
    pub fn contrast(&mut self, factor: f32) {
        let factor = factor.max(0.0).min(3.0);
        let intercept = 128.0 * (1.0 - factor);

        for i in (0..self.data.len()).step_by(4) {
            self.data[i] = ((self.data[i] as f32 * factor + intercept).max(0.0).min(255.0)) as u8;
            self.data[i + 1] = ((self.data[i + 1] as f32 * factor + intercept).max(0.0).min(255.0)) as u8;
            self.data[i + 2] = ((self.data[i + 2] as f32 * factor + intercept).max(0.0).min(255.0)) as u8;
        }
    }

    /// Apply box blur
    pub fn blur(&mut self, radius: usize) {
        let radius = radius.max(1).min(20);
        let original = self.data.clone();

        for y in 0..self.height {
            for x in 0..self.width {
                let mut r_sum = 0u32;
                let mut g_sum = 0u32;
                let mut b_sum = 0u32;
                let mut count = 0u32;

                for dy in -(radius as i32)..=(radius as i32) {
                    for dx in -(radius as i32)..=(radius as i32) {
                        let nx = (x as i32 + dx).max(0).min(self.width as i32 - 1) as usize;
                        let ny = (y as i32 + dy).max(0).min(self.height as i32 - 1) as usize;
                        let idx = (ny * self.width + nx) * 4;

                        r_sum += original[idx] as u32;
                        g_sum += original[idx + 1] as u32;
                        b_sum += original[idx + 2] as u32;
                        count += 1;
                    }
                }

                let idx = (y * self.width + x) * 4;
                self.data[idx] = (r_sum / count) as u8;
                self.data[idx + 1] = (g_sum / count) as u8;
                self.data[idx + 2] = (b_sum / count) as u8;
            }
        }
    }

    /// Apply edge detection (Sobel operator)
    pub fn edge_detect(&mut self) {
        let original = self.data.clone();

        // First convert to grayscale intensities
        let mut intensities = vec![0u8; self.width * self.height];
        for y in 0..self.height {
            for x in 0..self.width {
                let idx = (y * self.width + x) * 4;
                let r = original[idx] as f32;
                let g = original[idx + 1] as f32;
                let b = original[idx + 2] as f32;
                intensities[y * self.width + x] = (0.299 * r + 0.587 * g + 0.114 * b) as u8;
            }
        }

        // Apply Sobel operator
        for y in 1..(self.height - 1) {
            for x in 1..(self.width - 1) {
                let mut gx = 0i32;
                let mut gy = 0i32;

                // Sobel kernels
                for dy in -1i32..=1 {
                    for dx in -1i32..=1 {
                        let idx = ((y as i32 + dy) * self.width as i32 + (x as i32 + dx)) as usize;
                        let intensity = intensities[idx] as i32;

                        // Gx kernel: [-1, 0, 1], [-2, 0, 2], [-1, 0, 1]
                        gx += intensity * dx * (2 - dy.abs());
                        // Gy kernel: [-1, -2, -1], [0, 0, 0], [1, 2, 1]
                        gy += intensity * dy * (2 - dx.abs());
                    }
                }

                let magnitude = ((gx * gx + gy * gy) as f32).sqrt().min(255.0) as u8;
                let idx = (y * self.width + x) * 4;
                self.data[idx] = magnitude;
                self.data[idx + 1] = magnitude;
                self.data[idx + 2] = magnitude;
            }
        }
    }

    /// Apply sharpen filter
    pub fn sharpen(&mut self) {
        let original = self.data.clone();

        // Sharpen kernel: [0, -1, 0], [-1, 5, -1], [0, -1, 0]
        for y in 1..(self.height - 1) {
            for x in 1..(self.width - 1) {
                for c in 0..3 {
                    let center_idx = (y * self.width + x) * 4 + c;
                    let top_idx = ((y - 1) * self.width + x) * 4 + c;
                    let bottom_idx = ((y + 1) * self.width + x) * 4 + c;
                    let left_idx = (y * self.width + (x - 1)) * 4 + c;
                    let right_idx = (y * self.width + (x + 1)) * 4 + c;

                    let value = (original[center_idx] as i32 * 5
                        - original[top_idx] as i32
                        - original[bottom_idx] as i32
                        - original[left_idx] as i32
                        - original[right_idx] as i32)
                        .max(0)
                        .min(255);

                    self.data[center_idx] = value as u8;
                }
            }
        }
    }

    /// Apply pixelate effect
    pub fn pixelate(&mut self, block_size: usize) {
        let block_size = block_size.max(2).min(50);
        let original = self.data.clone();

        for block_y in (0..self.height).step_by(block_size) {
            for block_x in (0..self.width).step_by(block_size) {
                let mut r_sum = 0u32;
                let mut g_sum = 0u32;
                let mut b_sum = 0u32;
                let mut count = 0u32;

                // Calculate average color in block
                for y in block_y..block_y.saturating_add(block_size).min(self.height) {
                    for x in block_x..block_x.saturating_add(block_size).min(self.width) {
                        let idx = (y * self.width + x) * 4;
                        r_sum += original[idx] as u32;
                        g_sum += original[idx + 1] as u32;
                        b_sum += original[idx + 2] as u32;
                        count += 1;
                    }
                }

                let avg_r = (r_sum / count) as u8;
                let avg_g = (g_sum / count) as u8;
                let avg_b = (b_sum / count) as u8;

                // Apply average color to entire block
                for y in block_y..block_y.saturating_add(block_size).min(self.height) {
                    for x in block_x..block_x.saturating_add(block_size).min(self.width) {
                        let idx = (y * self.width + x) * 4;
                        self.data[idx] = avg_r;
                        self.data[idx + 1] = avg_g;
                        self.data[idx + 2] = avg_b;
                    }
                }
            }
        }
    }
}

/// Generate a test pattern image
#[wasm_bindgen]
pub fn generate_test_pattern(width: usize, height: usize) -> Vec<u8> {
    let mut data = vec![0u8; width * height * 4];

    for y in 0..height {
        for x in 0..width {
            let idx = (y * width + x) * 4;

            // Create a colorful gradient pattern
            let r = ((x as f32 / width as f32) * 255.0) as u8;
            let g = ((y as f32 / height as f32) * 255.0) as u8;
            let b = (((x + y) as f32 / (width + height) as f32) * 255.0) as u8;

            // Add some geometric shapes
            let center_x = width / 2;
            let center_y = height / 2;
            let dx = (x as i32 - center_x as i32).abs() as f32;
            let dy = (y as i32 - center_y as i32).abs() as f32;
            let dist = (dx * dx + dy * dy).sqrt();

            if dist < (width.min(height) / 4) as f32 {
                // Circle in center
                data[idx] = 255 - r;
                data[idx + 1] = 255 - g;
                data[idx + 2] = 255 - b;
            } else if x % 50 < 5 || y % 50 < 5 {
                // Grid pattern
                data[idx] = 200;
                data[idx + 1] = 200;
                data[idx + 2] = 200;
            } else {
                data[idx] = r;
                data[idx + 1] = g;
                data[idx + 2] = b;
            }

            data[idx + 3] = 255; // Alpha
        }
    }

    data
}
