/*
 * x86-64 Assembly HTTP Server for Switchback
 * Demonstrates SPA backend at the lowest possible level
 *
 * System calls used (x86-64 Linux):
 * - socket (41), bind (49), listen (50), accept (43)
 * - read (0), write (1), close (3), openat (257)
 * - setsockopt (54), exit (60)
 */

.section .data

/* Server configuration */
.align 4
port:           .word 0x401f        # Port 8000 in network byte order (0x1F40 reversed)

/* Socket address structure (sockaddr_in) */
.align 4
sockaddr:
    .word 2                         # sin_family = AF_INET (2)
    .word 0x401f                    # sin_port = 8000 (network byte order)
    .long 0                         # sin_addr = INADDR_ANY (0.0.0.0)
    .space 8                        # padding

/* /proc file paths */
proc_uptime:
    .ascii "/proc/uptime"
    .byte 0
proc_loadavg:
    .ascii "/proc/loadavg"
    .byte 0

/* HTTP Response templates */
http_200_header:
    .ascii "HTTP/1.1 200 OK\r\n"
    .ascii "Content-Type: application/json\r\n"
    .ascii "X-Switchback-Version: 1.0\r\n"
    .ascii "Connection: close\r\n"
    .ascii "\r\n"
http_200_len = . - http_200_header

http_200_html:
    .ascii "HTTP/1.1 200 OK\r\n"
    .ascii "Content-Type: text/html; charset=utf-8\r\n"
    .ascii "Connection: close\r\n"
    .ascii "\r\n"
http_200_html_len = . - http_200_html

http_404:
    .ascii "HTTP/1.1 404 Not Found\r\n"
    .ascii "Content-Type: text/plain\r\n"
    .ascii "Connection: close\r\n"
    .ascii "\r\n"
    .ascii "404 Not Found"
http_404_len = . - http_404

/* HTML index page */
index_html:
    .ascii "<!DOCTYPE html>\n"
    .ascii "<html>\n"
    .ascii "<head>\n"
    .ascii "  <meta charset=\"utf-8\">\n"
    .ascii "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
    .ascii "  <title>x86-64 Assembly + Switchback</title>\n"
    .ascii "</head>\n"
    .ascii "<body>\n"
    .ascii "  <div data-swbk-app></div>\n"
    .ascii "  <script>window.initialPage="
index_html_len_1 = . - index_html

index_html_2:
    .ascii "</script>\n"
    .ascii "  <script type=\"module\" src=\"/dist/app.js\"></script>\n"
    .ascii "</body>\n"
    .ascii "</html>\n"
index_html_2_len = . - index_html_2

/* JavaScript bundle path */
js_bundle:
    .ascii "dist/app.js"
    .byte 0

/* Path strings for matching */
path_root:      .ascii "/"
                .byte 0
path_about:     .ascii "/about"
                .byte 0
path_dist:      .ascii "/dist/app.js"
                .byte 0

/* Header to check */
header_switchback:
    .ascii "X-Switchback:"
    .byte 0
header_switchback_len = . - header_switchback - 1

/* JSON response parts */
home_json_start:
    .ascii "{\"component\":\"Dashboard\",\"props\":{\"uptime\":"
home_json_start_len = . - home_json_start

json_load_field:
    .ascii ",\"load\":"
json_load_field_len = . - json_load_field

home_json_end:
    .ascii ",\"deviceId\":\"ENV-x86-64-001\"},\"url\":\"/\"}"
home_json_end_len = . - home_json_end

about_json:
    .ascii "{\"component\":\"About\",\"props\":{\"version\":\"1.0.0\",\"backend\":\"x86-64 Assembly - Environmental Monitoring Station\",\"architecture\":\"x86-64\"},\"url\":\"/about\"}"
about_json_len = . - about_json

http_200_js:
    .ascii "HTTP/1.1 200 OK\r\n"
    .ascii "Content-Type: application/javascript; charset=utf-8\r\n"
    .ascii "Connection: close\r\n"
    .ascii "\r\n"
http_200_js_len = . - http_200_js

/* Messages */
msg_starting:   .ascii "x86-64 Assembly server starting on port 8000...\n"
msg_starting_len = . - msg_starting

msg_ready:      .ascii "Server ready! Visit http://localhost:8000\n"
msg_ready_len = . - msg_ready

msg_shutdown:   .ascii "Shutting down gracefully...\n"
msg_shutdown_len = . - msg_shutdown

/* Debug messages */
debug_before_accept:   .ascii "[DEBUG] Before accept\n"
debug_before_accept_len = . - debug_before_accept

debug_after_accept:   .ascii "[DEBUG] After accept\n"
debug_after_accept_len = . - debug_after_accept

debug_before_read:     .ascii "[DEBUG] Before read\n"
debug_before_read_len = . - debug_before_read

debug_after_read:     .ascii "[DEBUG] After read\n"
debug_after_read_len = . - debug_after_read

debug_handle:   .ascii "[DEBUG] Handling request\n"
debug_handle_len = . - debug_handle

debug_stats:    .ascii "[DEBUG] Reading system stats\n"
debug_stats_len = . - debug_stats

debug_build:    .ascii "[DEBUG] Building JSON\n"
debug_build_len = . - debug_build

debug_send:     .ascii "[DEBUG] Sending response\n"
debug_send_len = . - debug_send

debug_done:     .ascii "[DEBUG] Request done\n"
debug_done_len = . - debug_done

.section .bss
.align 8
server_fd:      .space 4
client_fd:      .space 4
request_buf:    .space 4096
response_buf:   .space 8192
js_file_buf:    .space 131072      # 128KB for JS file
proc_buf:       .space 256         # Buffer for reading /proc files
uptime_secs:    .space 8           # System uptime in seconds
load_avg:       .space 8           # Load average * 100
shutdown_flag:  .space 1           # Set to 1 when shutdown signal received
sigaction_buf:  .space 152         # Buffer for sigaction struct

.section .text
.global _start

/* Signal handler for graceful shutdown */
signal_handler:
    # Set shutdown flag
    movb $1, shutdown_flag(%rip)
    # Return from signal handler (kernel handles rt_sigreturn)
    ret

_start:
    # Initialize shutdown flag
    movb $0, shutdown_flag(%rip)
    /* Print startup message */
    mov $1, %rdi                    # stdout
    lea msg_starting(%rip), %rsi
    mov $msg_starting_len, %rdx
    mov $1, %rax                    # sys_write
    syscall

    /* Create socket: socket(AF_INET, SOCK_STREAM, 0) */
    mov $2, %rdi                    # AF_INET
    mov $1, %rsi                    # SOCK_STREAM
    mov $0, %rdx                    # protocol
    mov $41, %rax                   # sys_socket
    syscall

    /* Save socket fd */
    mov %eax, server_fd(%rip)

    /* Set SO_REUSEADDR option */
    mov %eax, %edi                  # socket fd
    mov $1, %rsi                    # SOL_SOCKET
    mov $2, %rdx                    # SO_REUSEADDR
    sub $16, %rsp
    movl $1, (%rsp)
    mov %rsp, %r10                  # optval = &one
    mov $4, %r8                     # optlen
    mov $54, %rax                   # sys_setsockopt
    syscall
    add $16, %rsp

    /* Bind socket: bind(fd, &sockaddr, sizeof(sockaddr)) */
    mov server_fd(%rip), %edi
    lea sockaddr(%rip), %rsi
    mov $16, %rdx
    mov $49, %rax                   # sys_bind
    syscall

    cmp $0, %rax
    jl error_exit

    /* Listen: listen(fd, backlog) */
    mov server_fd(%rip), %edi
    mov $10, %rsi                   # backlog
    mov $50, %rax                   # sys_listen
    syscall

    cmp $0, %rax
    jl error_exit

    /* Set up signal handler for SIGTERM (15) */
    lea sigaction_buf(%rip), %rsi
    mov $152, %rcx
    mov %rsi, %rdi
    xor %al, %al
    rep stosb

    lea sigaction_buf(%rip), %rsi
    lea signal_handler(%rip), %rax
    movq %rax, (%rsi)
    movq $0x10000000, 8(%rsi)

    mov $15, %rdi
    mov $0, %rdx
    mov $8, %r10
    mov $13, %rax
    syscall

    /* Set up signal handler for SIGINT (2) */
    lea sigaction_buf(%rip), %rsi
    mov $2, %rdi
    mov $0, %rdx
    mov $8, %r10
    mov $13, %rax
    syscall

    /* Print ready message */
    mov $1, %rdi                    # stdout
    lea msg_ready(%rip), %rsi
    mov $msg_ready_len, %rdx
    mov $1, %rax                    # sys_write
    syscall

accept_loop:
    /* Check shutdown flag */
    cmpb $0, shutdown_flag(%rip)
    jne graceful_shutdown

    /* Accept connection: accept(fd, NULL, NULL) */
    mov server_fd(%rip), %edi
    mov $0, %rsi                    # addr (NULL)
    mov $0, %rdx                    # addrlen (NULL)
    mov $43, %rax                   # sys_accept
    syscall

    cmp $0, %rax
    jl accept_loop                  # Error accepting, retry

    /* Save client fd */
    mov %eax, client_fd(%rip)

    /* Read request: read(client_fd, buf, sizeof(buf)) */
    mov client_fd(%rip), %edi
    lea request_buf(%rip), %rsi
    mov $4096, %rdx
    mov $0, %rax                    # sys_read
    syscall

    /* Parse request and send response */
    call handle_request

    /* Close client connection */
    mov client_fd(%rip), %edi
    mov $3, %rax                    # sys_close
    syscall

    jmp accept_loop

/* Graceful shutdown handler */
graceful_shutdown:
    /* Print shutdown message */
    mov $1, %rdi                    # stdout
    lea msg_shutdown(%rip), %rsi
    mov $msg_shutdown_len, %rdx
    mov $1, %rax                    # sys_write
    syscall

    /* Close server socket */
    mov server_fd(%rip), %edi
    mov $3, %rax                    # sys_close
    syscall

    /* Exit cleanly */
    mov $0, %rdi                    # exit code 0
    mov $60, %rax                   # sys_exit
    syscall

/* Handle HTTP request */
handle_request:
    push %rbp
    mov %rsp, %rbp
    push %rbx
    push %r12
    push %r13

    /* Check if it's a GET request */
    lea request_buf(%rip), %rax
    cmpb $'G', (%rax)
    jne send_404
    cmpb $'E', 1(%rax)
    jne send_404
    cmpb $'T', 2(%rax)
    jne send_404

    /* Find the path (skip "GET ") */
    add $4, %rax
    mov %rax, %r12                  # Save path pointer

    /* Find end of path (space) */
    mov %rax, %rbx
find_path_end:
    movb (%rbx), %cl
    cmp $' ', %cl
    je found_path_end
    cmp $'?', %cl                   # Query string
    je found_path_end
    inc %rbx
    jmp find_path_end

found_path_end:
    /* Check for X-Switchback header BEFORE null-terminating */
    push %rbx                       # Save path end pointer
    call check_switchback_header
    mov %rax, %r13                  # Save result (1 = has header, 0 = no header)
    pop %rbx                        # Restore path end pointer

    /* Now null-terminate path */
    movb $0, (%rbx)

    /* Route the request */
    mov %r12, %rax                  # Restore path pointer

    /* Check if path is "/" */
    cmpb $'/', (%rax)
    jne check_about
    cmpb $0, 1(%rax)
    je handle_root

check_about:
    /* Check if path is "/about" */
    lea path_about(%rip), %rsi
    mov %rax, %rdi
    call strcmp
    cmp $0, %rax
    je handle_about

check_js:
    /* Check if path is "/dist/app.js" */
    mov %r12, %rdi
    lea path_dist(%rip), %rsi
    call strcmp
    cmp $0, %rax
    je handle_js

    jmp send_404

handle_root:
    /* If X-Switchback header present, send JSON */
    cmp $1, %r13
    je send_home_json

    /* Otherwise send HTML */
    call send_html_with_json
    jmp request_done

send_home_json:
    /* Build and send JSON response for home with real stats */
    call build_home_json
    call send_json_response
    jmp request_done

handle_about:
    /* Send JSON response for about */
    lea about_json(%rip), %rdi
    mov $about_json_len, %rsi
    call send_json_response
    jmp request_done

handle_js:
    /* Send JavaScript bundle */
    call send_js_bundle
    jmp request_done

send_404:
    mov client_fd(%rip), %edi
    lea http_404(%rip), %rsi
    mov $http_404_len, %rdx
    mov $1, %rax                    # sys_write
    syscall

request_done:
    pop %r13
    pop %r12
    pop %rbx
    pop %rbp
    ret

/* Send JSON response */
/* rdi = JSON data pointer, rsi = length */
send_json_response:
    push %rbp
    mov %rsp, %rbp
    push %rbx
    push %r12

    mov %rdi, %rbx                  # Save JSON pointer
    mov %rsi, %r12                  # Save JSON length

    /* Send HTTP header */
    mov client_fd(%rip), %edi
    lea http_200_header(%rip), %rsi
    mov $http_200_len, %rdx
    mov $1, %rax                    # sys_write
    syscall

    /* Send JSON body */
    mov client_fd(%rip), %edi
    mov %rbx, %rsi
    mov %r12, %rdx
    mov $1, %rax                    # sys_write
    syscall

    pop %r12
    pop %rbx
    pop %rbp
    ret

/* Send HTML with embedded JSON */
send_html_with_json:
    push %rbp
    mov %rsp, %rbp
    push %rbx
    push %r12

    /* Send HTTP header */
    mov client_fd(%rip), %edi
    lea http_200_html(%rip), %rsi
    mov $http_200_html_len, %rdx
    mov $1, %rax
    syscall

    /* Send HTML part 1 */
    mov client_fd(%rip), %edi
    lea index_html(%rip), %rsi
    mov $index_html_len_1, %rdx
    mov $1, %rax
    syscall

    /* Build and send home JSON (for initialPage) */
    call build_home_json
    mov %rdi, %rbx
    mov %rsi, %r12
    mov client_fd(%rip), %edi
    mov %rbx, %rsi
    mov %r12, %rdx
    mov $1, %rax
    syscall

    /* Send HTML part 2 */
    mov client_fd(%rip), %edi
    lea index_html_2(%rip), %rsi
    mov $index_html_2_len, %rdx
    mov $1, %rax
    syscall

    pop %r12
    pop %rbx
    pop %rbp
    ret

/* Send JavaScript bundle from dist/app.js */
send_js_bundle:
    push %rbp
    mov %rsp, %rbp
    push %rbx
    push %r12

    /* Open file: openat(AT_FDCWD, "dist/app.js", O_RDONLY) */
    mov $-100, %rdi                 # AT_FDCWD
    lea js_bundle(%rip), %rsi
    mov $0, %rdx                    # O_RDONLY
    mov $257, %rax                  # sys_openat
    syscall

    cmp $0, %rax
    jl send_404_from_js

    mov %eax, %ebx                  # Save file fd

    /* Read file into buffer */
    mov %ebx, %edi
    lea js_file_buf(%rip), %rsi
    mov $131072, %rdx               # Read up to 128KB
    mov $0, %rax                    # sys_read
    syscall

    mov %rax, %r12                  # Save bytes read

    /* Close file */
    mov %ebx, %edi
    mov $3, %rax                    # sys_close
    syscall

    cmp $0, %r12
    jle send_404_from_js

    /* Send HTTP 200 header for JS */
    mov client_fd(%rip), %edi
    lea http_200_js(%rip), %rsi
    mov $http_200_js_len, %rdx
    mov $1, %rax                    # sys_write
    syscall

    /* Send JS file content */
    mov client_fd(%rip), %edi
    lea js_file_buf(%rip), %rsi
    mov %r12, %rdx                  # Bytes read
    mov $1, %rax
    syscall

    pop %r12
    pop %rbx
    pop %rbp
    ret

send_404_from_js:
    pop %r12
    pop %rbx
    pop %rbp
    jmp send_404

/* Check for X-Switchback header */
/* Returns: rax = 1 if header present, 0 otherwise */
check_switchback_header:
    push %rbp
    mov %rsp, %rbp
    push %rbx
    push %r12

    lea request_buf(%rip), %rbx
    lea header_switchback(%rip), %r12

search_header:
    /* Check for end of headers (double newline) */
    cmpb $0, (%rbx)
    je header_not_found
    cmpb $'\r', (%rbx)
    jne next_char
    cmpb $'\n', 1(%rbx)
    jne next_char
    cmpb $'\r', 2(%rbx)
    jne next_char
    cmpb $'\n', 3(%rbx)
    je header_not_found

next_char:
    /* Compare with header string */
    mov %rbx, %rdi
    mov %r12, %rsi
    mov $header_switchback_len, %rdx
    call strncmp
    cmp $0, %rax
    je header_found

    inc %rbx
    jmp search_header

header_found:
    mov $1, %rax
    pop %r12
    pop %rbx
    pop %rbp
    ret

header_not_found:
    mov $0, %rax
    pop %r12
    pop %rbx
    pop %rbp
    ret

/* String comparison */
/* rdi = str1, rsi = str2 */
/* Returns: rax = 0 if equal, non-zero otherwise */
strcmp:
    push %rbp
    mov %rsp, %rbp

strcmp_loop:
    movb (%rdi), %al
    movb (%rsi), %cl
    cmp %cl, %al
    jne strcmp_diff
    cmp $0, %al
    je strcmp_equal
    inc %rdi
    inc %rsi
    jmp strcmp_loop

strcmp_equal:
    mov $0, %rax
    pop %rbp
    ret

strcmp_diff:
    sub %cl, %al
    movzbl %al, %eax
    pop %rbp
    ret

/* String comparison (first n characters) */
/* rdi = str1, rsi = str2, rdx = n */
/* Returns: rax = 0 if equal, non-zero otherwise */
strncmp:
    push %rbp
    mov %rsp, %rbp

strncmp_loop:
    cmp $0, %rdx
    je strncmp_equal
    movb (%rdi), %al
    movb (%rsi), %cl
    cmp %cl, %al
    jne strncmp_diff
    dec %rdx
    inc %rdi
    inc %rsi
    jmp strncmp_loop

strncmp_equal:
    mov $0, %rax
    pop %rbp
    ret

strncmp_diff:
    sub %cl, %al
    movzbl %al, %eax
    pop %rbp
    ret

/* Read system statistics from /proc */
read_system_stats:
    push %rbp
    mov %rsp, %rbp
    push %rbx

    /* Read /proc/uptime */
    mov $-100, %rdi                 # AT_FDCWD
    lea proc_uptime(%rip), %rsi
    mov $0, %rdx                    # O_RDONLY
    mov $257, %rax                  # sys_openat
    syscall

    cmp $0, %rax
    jl read_stats_done

    mov %eax, %ebx                  # Save fd

    /* Read uptime data */
    mov %ebx, %edi
    lea proc_buf(%rip), %rsi
    mov $256, %rdx
    mov $0, %rax                    # sys_read
    syscall

    /* Close file */
    mov %ebx, %edi
    mov $3, %rax                    # sys_close
    syscall

    /* Parse uptime (first number before space) */
    lea proc_buf(%rip), %rdi
    call parse_integer
    mov %rax, uptime_secs(%rip)

    /* Read /proc/loadavg */
    mov $-100, %rdi                 # AT_FDCWD
    lea proc_loadavg(%rip), %rsi
    mov $0, %rdx                    # O_RDONLY
    mov $257, %rax                  # sys_openat
    syscall

    cmp $0, %rax
    jl read_stats_done

    mov %eax, %ebx                  # Save fd

    /* Read loadavg data */
    mov %ebx, %edi
    lea proc_buf(%rip), %rsi
    mov $256, %rdx
    mov $0, %rax                    # sys_read
    syscall

    /* Close file */
    mov %ebx, %edi
    mov $3, %rax                    # sys_close
    syscall

    /* Parse load average (first number, multiply by 100) */
    lea proc_buf(%rip), %rdi
    call parse_load_avg
    mov %rax, load_avg(%rip)

read_stats_done:
    pop %rbx
    pop %rbp
    ret

/* Parse integer from ASCII string */
/* rdi = string pointer */
/* Returns: rax = parsed integer */
parse_integer:
    push %rbp
    mov %rsp, %rbp

    mov $0, %rax                    # result = 0
    mov %rdi, %rsi                  # current char pointer

parse_int_loop:
    movb (%rsi), %cl                # load character
    cmp $'0', %cl
    jl parse_int_done
    cmp $'9', %cl
    jg parse_int_done

    sub $'0', %cl                   # convert to digit
    imul $10, %rax                  # result *= 10
    movzbl %cl, %ecx
    add %rcx, %rax                  # result += digit

    inc %rsi
    jmp parse_int_loop

parse_int_done:
    pop %rbp
    ret

/* Parse load average and multiply by 100 */
/* rdi = string pointer */
/* Returns: rax = load * 100 */
parse_load_avg:
    push %rbp
    mov %rsp, %rbp
    push %rbx
    push %r12

    mov %rdi, %rbx                  # Save string pointer
    call parse_integer              # Parse integer part
    mov %rax, %r12                  # Save integer part

    /* Find decimal point */
    mov %rbx, %rdi
find_decimal:
    movb (%rdi), %cl
    cmp $'.', %cl
    je found_decimal
    cmp $0, %cl
    je load_done
    cmp $' ', %cl
    je load_done
    inc %rdi
    jmp find_decimal

found_decimal:
    inc %rdi                        # Skip '.'
    /* Parse first two decimal digits */
    movb (%rdi), %cl                # First decimal digit
    cmp $'0', %cl
    jl load_done
    cmp $'9', %cl
    jg load_done
    sub $'0', %cl
    movzbl %cl, %eax
    imul $10, %rax                  # decimal_part = first_digit * 10

    inc %rdi
    movb (%rdi), %cl                # Second decimal digit
    cmp $'0', %cl
    jl add_decimal
    cmp $'9', %cl
    jg add_decimal
    sub $'0', %cl
    movzbl %cl, %ecx
    add %rcx, %rax                  # decimal_part += second_digit

add_decimal:
    imul $100, %r12                 # integer_part * 100
    add %r12, %rax                  # + decimal_part
    mov %rax, %r12

load_done:
    mov %r12, %rax
    pop %r12
    pop %rbx
    pop %rbp
    ret

/* Build Home JSON response with system stats */
/* Returns: rdi = buffer pointer, rsi = length */
build_home_json:
    push %rbp
    mov %rsp, %rbp
    push %rbx

    /* Read system stats first */
    call read_system_stats

    /* Build JSON in response_buf */
    lea response_buf(%rip), %rdi

    /* {"component":"Dashboard","props":{"uptime": */
    lea home_json_start(%rip), %rsi
    mov $home_json_start_len, %rcx
    rep movsb

    /* Append uptime value */
    mov uptime_secs(%rip), %rsi
    call int_to_ascii

    /* ,"load": */
    lea json_load_field(%rip), %rsi
    mov $json_load_field_len, %rcx
    rep movsb

    /* Append load value */
    mov load_avg(%rip), %rsi
    call int_to_ascii

    /* ,"deviceId":"ENV-x86-64-001"},"url":"/"} */
    lea home_json_end(%rip), %rsi
    mov $home_json_end_len, %rcx
    rep movsb

    /* Calculate length */
    lea response_buf(%rip), %rbx
    sub %rbx, %rdi
    mov %rdi, %rsi                  # length
    mov %rbx, %rdi                  # buffer pointer

    pop %rbx
    pop %rbp
    ret

/* Convert integer to ASCII and append to buffer */
/* rdi = buffer position, rsi = integer value */
/* Returns: rdi = new buffer position */
int_to_ascii:
    push %rbp
    mov %rsp, %rbp
    push %rbx
    push %r12
    push %r13
    push %r14

    mov %rdi, %rbx                  # Save buffer pointer
    mov %rsi, %r12                  # Save value

    /* Handle 0 specially */
    cmp $0, %r12
    jne not_zero
    movb $'0', (%rbx)
    inc %rbx
    mov %rbx, %rdi
    pop %r14
    pop %r13
    pop %r12
    pop %rbx
    pop %rbp
    ret

not_zero:
    /* Count digits */
    mov %r12, %rax
    mov $0, %r13                    # Digit count
count_digits:
    cmp $0, %rax
    je digits_counted
    mov $0, %rdx
    mov $10, %rcx
    div %rcx
    inc %r13
    jmp count_digits

digits_counted:
    /* Write digits from right to left */
    add %r13, %rbx                  # Move to end
    mov %rbx, %rdi                  # Save end position

write_digits:
    cmp $0, %r13
    je digits_done
    mov $0, %rdx
    mov %r12, %rax
    mov $10, %rcx
    div %rcx                        # quotient in rax, remainder in rdx
    add $'0', %dl                   # Convert to ASCII
    dec %rbx
    movb %dl, (%rbx)
    mov %rax, %r12                  # value = quotient
    dec %r13
    jmp write_digits

digits_done:
    pop %r14
    pop %r13
    pop %r12
    pop %rbx
    pop %rbp
    ret

error_exit:
    mov $1, %rdi
    mov $60, %rax                   # sys_exit
    syscall
