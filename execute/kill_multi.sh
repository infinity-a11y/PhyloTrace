#!/bin/bash
unset R_HOME

log_file='execute/script_log.txt'
TARGET_SCRIPT='execute/kma_multi.sh'
# Function to log messages to the file
log_message() {
    echo "$(date +"%Y-%m-%d %H:%M:%S") - $1" >> "$log_file"
}
# Find the process ID (PID) of the script
PID=$(pgrep -f "$TARGET_SCRIPT")
if [ -z "$PID" ]; then
  echo "No process found for $TARGET_SCRIPT"
else
  # Kill the process
  echo "Killing process $PID for $TARGET_SCRIPT"
  kill "$PID"
fi
echo 0 > $log_file