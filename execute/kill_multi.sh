#!/bin/bash
unset R_HOME

log_file='execute/script_log.txt'
kma_multi='execute/kma_multi.sh'
automatic_typing='automatic_typing.R'

# Function to log messages to the file
log_message() {
    echo "$(date +"%Y-%m-%d %H:%M:%S") - $1" >> "$log_file"
}

# Find the process ID (PID) of the script
PID=$(pgrep -f "$kma_multi")
if [ -z "$PID" ]; then
  echo "No process found for $kma_multi"
else
  # Kill the process
  echo "Killing process $PID for $kma_multi"
  kill "$PID"
fi

# Find the process ID (PID) of the script
PID=$(pgrep -f "$automatic_typing")
if [ -z "$PID" ]; then
  echo "No process found for $automatic_typing"
else
  # Kill the process
  echo "Killing process $PID for $automatic_typing"
  kill "$PID"
fi

echo 0 > $log_file