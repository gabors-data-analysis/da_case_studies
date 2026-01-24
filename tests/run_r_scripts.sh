#!/bin/bash

# Get the directory where this script resides
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Set the starting directory to the parent of the script's directory
START_DIR="$(dirname "$SCRIPT_DIR")"

# Create output file with timestamp
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
OUTPUT_FILE="$SCRIPT_DIR/r_scripts_results_$TIMESTAMP.log"

echo "Starting execution of all R scripts in directory: $START_DIR"
echo "Results will be written to: $OUTPUT_FILE"

# Initialize the output file
echo "R Script Execution Results - $(date)" > "$OUTPUT_FILE"
echo "========================================" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"

# Counters for summary
SUCCESS_COUNT=0
FAIL_COUNT=0

# Find all files ending in .R or .r recursively from the starting directory
# Sort them to ensure consistent execution order (some scripts depend on others)
# and execute each one using Rscript
find "$START_DIR" -type f \( -name "*.R" -o -name "*.r" \) | sort | while IFS= read -r r_script; do
  echo "--- Executing: $r_script ---"
  
  # Run the script and capture both stdout and stderr
  ERROR_OUTPUT=$(Rscript "$r_script" 2>&1)
  EXIT_CODE=$?
  
  # Check if the error is just a display/viewer issue (not a real failure)
  IS_DISPLAY_ERROR=false
  if [ $EXIT_CODE -ne 0 ]; then
    if echo "$ERROR_OUTPUT" | grep -q "unable to start data viewer\|unable to open display"; then
      IS_DISPLAY_ERROR=true
    fi
  fi
  
  # Check if the Rscript command was successful or failed only due to display issues
  if [ $EXIT_CODE -eq 0 ] || [ "$IS_DISPLAY_ERROR" = true ]; then
    echo "--- Successfully executed: $r_script ---"
    if [ "$IS_DISPLAY_ERROR" = true ]; then
      echo "[SUCCESS] $r_script (display/viewer error ignored)" >> "$OUTPUT_FILE"
    else
      echo "[SUCCESS] $r_script" >> "$OUTPUT_FILE"
    fi
    ((SUCCESS_COUNT++))
  else
    echo "--- Failed to execute: $r_script ---"
    echo "[FAILED] $r_script" >> "$OUTPUT_FILE"
    echo "  Exit code: $EXIT_CODE" >> "$OUTPUT_FILE"
    echo "  Error message:" >> "$OUTPUT_FILE"
    echo "$ERROR_OUTPUT" | sed 's/^/    /' >> "$OUTPUT_FILE"
    echo "" >> "$OUTPUT_FILE"
    ((FAIL_COUNT++))
  fi
done

# Write summary to output file
echo "" >> "$OUTPUT_FILE"
echo "========================================" >> "$OUTPUT_FILE"
echo "SUMMARY" >> "$OUTPUT_FILE"
echo "Successful: $SUCCESS_COUNT" >> "$OUTPUT_FILE"
echo "Failed: $FAIL_COUNT" >> "$OUTPUT_FILE"
echo "Completed at: $(date)" >> "$OUTPUT_FILE"

echo "Finished execution of all R scripts."
echo "Results written to: $OUTPUT_FILE"