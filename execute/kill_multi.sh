#!/bin/bash
unset R_HOME

log_file='execute/script_log.txt'
blat_multi='execute/blat_multi.sh'
automatic_typing='automatic_typing.R'

# Function to log messages to the file
log_message() {
    echo "$(date +"%Y-%m-%d %H:%M:%S") - $1" >> "$log_file"
}

# Find the process ID (PID) of the script
PID=$(pgrep -f $blat_multi)
if [ -z "$PID" ]; then
  echo "No process found for $blat_multi"
else
  # Kill the process
  echo "Killing process $PID for $blat_multi"
  kill "$PID"
fi

# Kill parallel
killall -TERM perl

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