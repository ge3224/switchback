const std = @import("std");
const mem = std.mem;
const fs = std.fs;
const posix = std.posix;

// Global server state
var server_running = std.atomic.Value(bool).init(true);
var server_fd_global: posix.fd_t = -1;

// Global submission counter (in-memory, resets on restart)
var submission_count: u32 = 0;
var submission_mutex: std.Thread.Mutex = .{};

fn serveStatic(stream: std.net.Stream, path: []const u8) !void {
    const file = fs.cwd().openFile(path, .{}) catch {
        const response = "HTTP/1.1 404 Not Found\r\nContent-Length: 13\r\n\r\n404 Not Found";
        _ = try stream.write(response);
        return;
    };
    defer file.close();

    const content = try file.readToEndAlloc(std.heap.page_allocator, 10 * 1024 * 1024);
    defer std.heap.page_allocator.free(content);

    // Determine content type
    const content_type = if (mem.endsWith(u8, path, ".js"))
        "application/javascript"
    else if (mem.endsWith(u8, path, ".css"))
        "text/css"
    else if (mem.endsWith(u8, path, ".html"))
        "text/html"
    else
        "application/octet-stream";

    const header = try std.fmt.allocPrint(std.heap.page_allocator, "HTTP/1.1 200 OK\r\nContent-Type: {s}\r\nContent-Length: {d}\r\n\r\n", .{ content_type, content.len });
    defer std.heap.page_allocator.free(header);

    _ = try stream.write(header);
    _ = try stream.write(content);
}

fn respondJson(stream: std.net.Stream, json: []const u8) !void {
    const header = try std.fmt.allocPrint(std.heap.page_allocator, "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: {d}\r\n\r\n", .{json.len});
    defer std.heap.page_allocator.free(header);

    _ = try stream.write(header);
    _ = try stream.write(json);
}

fn respondHtml(allocator: mem.Allocator, stream: std.net.Stream, page_json: []const u8) !void {
    const html = try std.fmt.allocPrint(allocator,
        \\<!DOCTYPE html>
        \\<html lang="en">
        \\<head>
        \\    <meta charset="UTF-8">
        \\    <meta name="viewport" content="width=device-width, initial-scale=1.0">
        \\    <title>Zig Recipe - Switchback</title>
        \\    <script>window.initialPage = {s};</script>
        \\</head>
        \\<body>
        \\    <div data-swbk-app>
        \\        <div style="padding: 2rem; text-align: center;">
        \\            <p>Loading Switchback app...</p>
        \\        </div>
        \\    </div>
        \\    <script type="module" src="/dist/app.js"></script>
        \\</body>
        \\</html>
    , .{page_json});
    defer allocator.free(html);

    const header = try std.fmt.allocPrint(std.heap.page_allocator, "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: {d}\r\n\r\n", .{html.len});
    defer std.heap.page_allocator.free(header);

    _ = try stream.write(header);
    _ = try stream.write(html);
}

// Escape string for JSON
fn jsonEscape(allocator: mem.Allocator, input: []const u8) ![]const u8 {
    var result = try allocator.alloc(u8, input.len * 2); // Worst case: all chars escaped
    var result_len: usize = 0;

    for (input) |c| {
        switch (c) {
            '"' => {
                result[result_len] = '\\';
                result[result_len + 1] = '"';
                result_len += 2;
            },
            '\\' => {
                result[result_len] = '\\';
                result[result_len + 1] = '\\';
                result_len += 2;
            },
            '\n' => {
                result[result_len] = '\\';
                result[result_len + 1] = 'n';
                result_len += 2;
            },
            '\r' => {
                result[result_len] = '\\';
                result[result_len + 1] = 'r';
                result_len += 2;
            },
            '\t' => {
                result[result_len] = '\\';
                result[result_len + 1] = 't';
                result_len += 2;
            },
            else => {
                result[result_len] = c;
                result_len += 1;
            },
        }
    }

    return allocator.realloc(result, result_len);
}

// Basic URL decoder - simple version that works across Zig versions
fn urlDecode(allocator: mem.Allocator, encoded: []const u8) ![]const u8 {
    // Allocate maximum possible size (worst case: all characters stay same)
    var result = try allocator.alloc(u8, encoded.len);
    var result_len: usize = 0;

    var i: usize = 0;
    while (i < encoded.len) {
        if (encoded[i] == '+') {
            result[result_len] = ' ';
            result_len += 1;
            i += 1;
        } else if (encoded[i] == '%' and i + 2 < encoded.len) {
            const hex = encoded[i + 1 .. i + 3];
            const byte = std.fmt.parseInt(u8, hex, 16) catch {
                result[result_len] = encoded[i];
                result_len += 1;
                i += 1;
                continue;
            };
            result[result_len] = byte;
            result_len += 1;
            i += 3;
        } else {
            result[result_len] = encoded[i];
            result_len += 1;
            i += 1;
        }
    }

    // Shrink to actual size
    return allocator.realloc(result, result_len);
}

fn handleSignal(sig: c_int) callconv(.C) void {
    _ = sig;
    std.debug.print("\n⚡ Shutting down gracefully...\n", .{});
    server_running.store(false, .seq_cst);

    // Close server socket to unblock accept()
    if (server_fd_global >= 0) {
        posix.close(server_fd_global);
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Install signal handlers
    const act = posix.Sigaction{
        .handler = .{ .handler = handleSignal },
        .mask = posix.empty_sigset,
        .flags = 0,
    };
    try posix.sigaction(posix.SIG.TERM, &act, null);
    try posix.sigaction(posix.SIG.INT, &act, null);

    const address = try std.net.Address.parseIp("0.0.0.0", 8000);
    var server = try address.listen(.{});
    defer server.deinit();

    // Store socket fd globally for signal handler
    server_fd_global = server.stream.handle;

    std.debug.print("⚡ Zig server listening on http://0.0.0.0:8000\n", .{});
    std.debug.print("   Try form submissions at /submit!\n", .{});

    while (server_running.load(.seq_cst)) {
        // Accept connection (will be unblocked by signal handler closing socket)
        const connection = server.accept() catch |err| {
            if (!server_running.load(.seq_cst)) {
                // Server is shutting down
                break;
            }
            // Actual error - propagate it
            return err;
        };
        const thread = try std.Thread.spawn(.{}, handleConnection, .{ allocator, connection });
        thread.detach();
    }

    std.debug.print("⚡ Server stopped\n", .{});
}

fn handleConnection(allocator: mem.Allocator, connection: std.net.Server.Connection) void {
    defer connection.stream.close();
    handleRequest(allocator, connection) catch |err| {
        std.debug.print("Error handling request: {}\n", .{err});
    };
}

fn handleRequest(allocator: mem.Allocator, connection: std.net.Server.Connection) !void {
    var read_buffer: [8192]u8 = undefined;
    const bytes_read = try connection.stream.read(&read_buffer);
    const request_data = read_buffer[0..bytes_read];

    // Parse request line
    var lines = mem.splitSequence(u8, request_data, "\r\n");
    const request_line = lines.next() orelse return error.InvalidRequest;

    var parts = mem.splitSequence(u8, request_line, " ");
    const method_str = parts.next() orelse return error.InvalidRequest;
    const uri = parts.next() orelse return error.InvalidRequest;

    const is_post = mem.eql(u8, method_str, "POST");

    // Check for X-Switchback header and find body
    var is_switchback = false;
    var content_length: usize = 0;

    while (lines.next()) |line| {
        if (line.len == 0) break; // End of headers
        if (mem.startsWith(u8, line, "X-Switchback:") or mem.startsWith(u8, line, "x-switchback:")) {
            is_switchback = true;
        }
        if (mem.startsWith(u8, line, "Content-Length:")) {
            const len_str = mem.trim(u8, line[15..], " \t");
            content_length = std.fmt.parseInt(usize, len_str, 10) catch 0;
        }
    }

    // Get body for POST requests
    var form_name: []const u8 = "";
    var form_email: []const u8 = "";
    if (is_post) {
        // The body is everything after the blank line
        const body = lines.rest();

        // Parse multipart form data (used by Switchback FormData)
        // Look for field patterns like: name="fieldname"\n\nvalue
        var body_iter = mem.splitSequence(u8, body, "\n");
        var current_field: []const u8 = "";
        var in_value = false;

        while (body_iter.next()) |line| {
            // Check for field name
            if (mem.indexOf(u8, line, "name=\"") != null) {
                const start = mem.indexOf(u8, line, "name=\"").? + 6;
                const end = mem.indexOfPos(u8, line, start, "\"") orelse line.len;
                current_field = line[start..end];
                in_value = false;
            } else if (line.len <= 1 and current_field.len > 0) {
                // Empty line (or just \r) means next line is the value
                in_value = true;
            } else if (in_value and !mem.startsWith(u8, line, "------")) {
                // This is the value line
                if (mem.eql(u8, current_field, "name")) {
                    form_name = try allocator.dupe(u8, line);
                } else if (mem.eql(u8, current_field, "email")) {
                    form_email = try allocator.dupe(u8, line);
                }
                in_value = false;
                current_field = "";
            }
        }
    }

    // Serve static files
    if (mem.startsWith(u8, uri, "/dist/")) {
        try serveStatic(connection.stream, uri[1..]);
        return;
    }

    // Build JSON response
    var json: []const u8 = undefined;

    if (mem.eql(u8, uri, "/")) {
        submission_mutex.lock();
        const count = submission_count;
        submission_mutex.unlock();

        json = try std.fmt.allocPrint(allocator,
            \\{{"component":"Home","props":{{"message":"A blazingly fast Zig backend with Switchback form handling","stats":{{"submissions":{d},"framework":"Zig 0.13"}}}},"url":"/"}}
        , .{count});
    } else if (mem.eql(u8, uri, "/submit") and !is_post) {
        json = try std.fmt.allocPrint(allocator,
            \\{{"component":"Submit","props":{{"recentSubmissions":[]}},"url":"/submit"}}
        , .{});
    } else if (mem.eql(u8, uri, "/submit") and is_post) {
        submission_mutex.lock();
        submission_count += 1;
        submission_mutex.unlock();

        std.debug.print("📝 Form submitted: {s} ({s})\n", .{ form_name, form_email });

        // Escape form data for JSON
        const escaped_name = try jsonEscape(allocator, form_name);
        defer allocator.free(escaped_name);
        const escaped_email = try jsonEscape(allocator, form_email);
        defer allocator.free(escaped_email);

        json = try std.fmt.allocPrint(allocator,
            \\{{"component":"Submit/Success","props":{{"message":"Your form was processed by Zig!","submitted":{{"name":"{s}","email":"{s}"}}}},"url":"/submit"}}
        , .{ escaped_name, escaped_email });
    } else if (mem.eql(u8, uri, "/about")) {
        json = try std.fmt.allocPrint(allocator,
            \\{{"component":"About","props":{{"version":"1.0.0","backend":"Zig 0.13","features":["Form handling with POST requests","Multi-threaded HTTP server","Zero-cost abstractions","Compile-time safety","No external dependencies"]}},"url":"/about"}}
        , .{});
    } else {
        json = try std.fmt.allocPrint(allocator,
            \\{{"component":"Error","props":{{"message":"Page not found"}},"url":"{s}"}}
        , .{uri});
    }

    defer allocator.free(json);

    if (is_switchback) {
        try respondJson(connection.stream, json);
    } else {
        try respondHtml(allocator, connection.stream, json);
    }
}
