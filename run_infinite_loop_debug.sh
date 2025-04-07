#!/bin/bash

COMMAND="dune exec -- rescriptdep --format=dot --module FMTextInput /Users/woonki/GitHub/gl/farmmorning-app2/lib/bs"
REPEAT=20       # Number of desired repetitions
WAIT_SEC=5      # Seconds to wait before attaching debugger (for infinite loop detection)

for i in $(seq 1 $REPEAT); do
  echo "Execution count #$i..."

  # Run command in background
  $COMMAND &
  CMD_PID=$!

  echo "[DEBUG] Process PID: $CMD_PID"

  # Check if process has terminated every second for WAIT_SEC seconds
  for ((s=1; s<=$WAIT_SEC; s++)); do
    sleep 1
    # Exit loop if process has terminated
    if ! kill -0 $CMD_PID 2>/dev/null; then
      echo "Normal termination (execution completed)."
      break
    fi
  done

  # If process is still alive after WAIT_SEC seconds,
  # assume it's stuck in an infinite loop and attach lldb for debugging
  if kill -0 $CMD_PID 2>/dev/null; then
    echo "Process has not terminated (possible infinite loop)."
    echo "Tracing infinite loop with lldb..."
    lldb -p $CMD_PID -o "thread backtrace all" -o "quit"
    # Skip kill -9 if you don't want to terminate the process
    # Here we will terminate directly after lldb ends
    kill -9 $CMD_PID
    break
  else
    echo "Process terminated normally (execution count #$i completed)."
  fi
done