# x86-64 Assembly Demo - Switchback Integration

The most extreme demonstration of Switchback's backend-agnostic philosophy: a complete HTTP server written in **pure x86-64 assembly language** with direct Linux syscalls.

## What's Included

- **server.s** - x86-64 assembly HTTP server (~870 lines of hand-crafted assembly)
- **app.ts** - Client-side Switchback app with terminal HUD UI
- **vite.config.ts** - Bundles app.ts + Switchback into single JS file
- **Docker setup** - Multi-stage build for x86-64 architecture

## Why This Matters

This demo proves that Switchback works with **literally any backend**. We're not using:
- ❌ No web framework
- ❌ No standard library
- ❌ No runtime environment
- ❌ No dependencies whatsoever

Just raw machine code talking directly to the Linux kernel through syscalls.

## Quick Start

The easiest way to run this demo is with Docker (works on both x86-64 and ARM64 hosts via emulation):

```bash
cd examples/demos/asm
docker-compose up
```

Open http://localhost:8000

## What Happens Under the Hood

### System Calls Used

The assembly server uses these Linux syscalls:

| Syscall | Number | Purpose |
|---------|--------|---------|
| `socket` | 198 | Create TCP socket |
| `bind` | 200 | Bind socket to port 8000 |
| `listen` | 201 | Listen for connections |
| `accept` | 202 | Accept incoming connections |
| `read` | 63 | Read HTTP request data |
| `write` | 64 | Send HTTP responses |
| `openat` | 56 | Open files (for serving app.js) |
| `close` | 57 | Close file descriptors |
| `setsockopt` | 208 | Set SO_REUSEADDR option |
| `exit` | 93 | Terminate process |

### x86-64 Assembly Specifics

x86-64 uses the System V AMD64 ABI calling convention:

**Registers:**
- `rax-rdx, rsi, rdi, rsp, rbp, r8-r15`: 64-bit general purpose registers
- `rax`: Syscall number and return value
- `rdi, rsi, rdx, r10, r8, r9`: Syscall arguments (in order)
- `rbp`: Frame pointer
- `rsp`: Stack pointer

**Syscall Convention:**
```asm
mov $arg1, %rdi     # First argument
mov $arg2, %rsi     # Second argument
mov $arg3, %rdx     # Third argument
mov $syscall_num, %rax  # Syscall number
syscall             # Invoke syscall
# Return value in rax
```

**Example - Creating a Socket:**
```asm
mov $2, %rdi        # AF_INET
mov $1, %rsi        # SOCK_STREAM
mov $0, %rdx        # protocol
mov $41, %rax       # sys_socket
syscall             # Make syscall
# Socket fd returned in rax
```

## HTTP Request Processing Flow

1. **Create & Bind Socket** → Listen on port 8000
2. **Accept Loop** → Wait for connections
3. **Read Request** → Read up to 4KB into buffer
4. **Parse HTTP Method** → Check for "GET"
5. **Extract Path** → Parse URL path
6. **Check Headers** → Look for `X-Switchback` header
7. **Route Request:**
   - `/` → Send HTML (if no X-Switchback) or Home JSON
   - `/about` → Send About JSON
   - `/dist/app.js` → Serve JavaScript bundle
   - Other → Send 404
8. **Build Response** → Construct HTTP headers + body
9. **Send Response** → Write to socket
10. **Close Connection** → Clean up and loop

## Routes Implemented

### `GET /`
**Without X-Switchback header:**
```http
HTTP/1.1 200 OK
Content-Type: text/html

<!DOCTYPE html>
<html>
  <body>
    <div data-swbk-app></div>
    <script>window.initialPage={...home json...}</script>
    <script type="module" src="/dist/app.js"></script>
  </body>
</html>
```

**With X-Switchback header:**
```http
HTTP/1.1 200 OK
Content-Type: application/json

{"component":"Home","props":{...},"url":"/"}
```

### `GET /about`
```http
HTTP/1.1 200 OK
Content-Type: application/json

{"component":"About","props":{...},"url":"/about"}
```

### `GET /dist/app.js`
```http
HTTP/1.1 200 OK
Content-Type: application/javascript

/* bundled JavaScript code */
```

## Code Structure

### Memory Layout

```
.section .data       - Read-only data (strings, constants)
.section .bss        - Uninitialized data (buffers)
.section .text       - Executable code
```

### Key Functions

- `_start` - Entry point, creates socket and enters accept loop
- `handle_request` - Parses HTTP request and routes to handlers
- `check_switchback_header` - Searches for X-Switchback header
- `send_json_response` - Sends JSON with proper HTTP headers
- `send_html_with_json` - Sends initial HTML page
- `send_js_bundle` - Opens and serves JavaScript file
- `strcmp` / `strncmp` - String comparison utilities

## Performance Notes

Assembly provides:
- **Tiny binary size:** ~50KB executable (vs. megabytes for typical servers)
- **Minimal memory:** ~5MB runtime footprint
- **Zero startup time:** No runtime initialization
- **Direct syscalls:** No library overhead
- **Predictable performance:** Every instruction is explicit

## Running Natively (Advanced)

On any x86-64 Linux system:

```bash
# Assemble
as -o server.o server.s

# Link
ld -o server server.o

# Run
./server
```

Then build the frontend:
```bash
pnpm install
pnpm build
```

Visit http://localhost:8000

## Limitations

This is a **demonstration**, not production-ready code:

- ⚠️ Single-threaded (handles one connection at a time)
- ⚠️ Fixed buffer sizes (potential overflow)
- ⚠️ No input validation or sanitization
- ⚠️ Simple string parsing (not robust)
- ⚠️ No HTTPS/TLS support
- ⚠️ No authentication
- ⚠️ Hardcoded responses

## Educational Value

This demo teaches:
- How HTTP servers work at the lowest level
- ARM64 assembly programming
- Linux syscall interface
- Socket programming
- Binary protocol parsing
- Memory management without a runtime

## Comparison with Other Demos

| Demo | Language | Level | Lines of Code | Binary Size |
|------|----------|-------|---------------|-------------|
| **Assembly** | x86-64 asm | Bare metal | ~870 | ~8KB |
| C | C99 | Systems | ~800 | ~200KB |
| Rust | Rust | Systems | ~300 | ~3MB |
| Go | Go | High-level | ~150 | ~7MB |
| PHP | PHP | Scripting | ~100 | N/A (interpreted) |

The assembly demo has the smallest binary and most explicit control, but requires the most code for the same functionality.

## Docker Native Build

The Docker setup builds natively on x86-64:

- No emulation required
- Fast builds
- Runs on any x86-64 Linux system

## Debugging Tips

**View server logs:**
```bash
docker-compose logs -f
```

**Access running container:**
```bash
docker exec -it asm-asm-1 sh
```

**Check if server is running:**
```bash
ps aux | grep server
```

**Test HTTP manually:**
```bash
curl -v http://localhost:8000/
curl -H "X-Switchback: true" http://localhost:8000/
curl http://localhost:8000/about
```

## Troubleshooting

**Build fails:**
- Ensure you're on an x86-64 system: `uname -m` should show `x86_64`
- Check Docker is running: `docker ps`

**Server starts but can't connect:**
- Check port 8000 isn't in use: `lsof -i :8000`
- Try rebuilding: `docker-compose build --no-cache`

**JavaScript 404 errors:**
- Ensure `dist/app.js` was built in the Docker image
- Check build logs: `docker-compose build`

**Assembly errors:**
- The `.s` file uses GNU as (gas) AT&T syntax for x86-64
- Ensure you're using GNU binutils assembler

## Learn More About x86-64 Assembly

- [AMD64 ABI Reference](https://refspecs.linuxbase.org/elf/x86_64-abi-0.99.pdf)
- [x86-64 Linux Syscalls](https://blog.rchapman.org/posts/Linux_System_Call_Table_for_x86_64/)
- [x86-64 Assembly Tutorial](https://cs.lmu.edu/~ray/notes/nasmtutorial/)
- [Intel Software Developer Manual](https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html)

## Why x86-64?

We chose x86-64 because:
1. **Universal compatibility:** Runs on virtually all servers and desktops
2. **No emulation needed:** Native execution on most development machines
3. **Well-documented:** Decades of resources and tools
4. **Educational value:** Understanding the dominant server architecture

## Contributing

Found a bug or want to improve the assembly code? PRs welcome!

Potential improvements:
- Add multi-threading (clone syscall)
- Implement POST request handling
- Add proper error pages
- Optimize string parsing
- Add request logging
- Implement keep-alive connections

## License

Part of the Switchback project. See root LICENSE file.

---

**Bottom line:** If Switchback works with raw ARM64 assembly, it works with absolutely anything. Choose your backend, any backend.
