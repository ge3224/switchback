#!/bin/sh
# Entrypoint script with proper signal handling for PHP built-in server

# Trap SIGTERM and forward it to PHP
trap 'kill -TERM $PID' TERM INT

# Start PHP built-in server in background
php -S 0.0.0.0:8000 -t . index.php &
PID=$!

# Wait for PHP to exit
wait $PID
