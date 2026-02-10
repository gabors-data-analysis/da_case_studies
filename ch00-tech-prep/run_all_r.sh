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

# Exclusion patterns for find command
EXCLUDE_PATHS=(
  -path "*/ch00-tech-prep/*"
  -o -path "*/renv/*"
  -o -path "*/ch16-airbnb-random-forest/ch16-airbnb-random-forest.R"
  -o -path "*/ch17-predicting-firm-exit/ch17-predicting-firm-exit.R"
  -o -path "*/ch11-smoking-health-risk/ch11-smoking-health-risk-01-munging.R"
  -o -path "*/ch11-smoking-health-risk/ch11-smoking-health-risk-02-analysis.R"
)

# Find all files ending in .R or .r recursively from the starting directory
# Sort them to ensure consistent execution order (some scripts depend on others)
# Prioritize *-prepare.R, *-munging.R, *-setup.R, *-dataprep.R, *-maker.R files first, then alphabetical
scripts=$(
  {
    find "$START_DIR" -type f \( -name "*.R" -o -name "*.r" \) \
      ! \( "${EXCLUDE_PATHS[@]}" \) | grep -E '(prepare|munging|setup|dataprep|maker)' || true
    find "$START_DIR" -type f \( -name "*.R" -o -name "*.r" \) \
      ! \( "${EXCLUDE_PATHS[@]}" \) | grep -vE '(prepare|munging|setup|dataprep|maker)' || true
  } | sort
)

while IFS= read -r r_script; do
  [ -z "$r_script" ] && continue
  
  echo "--- Executing: $r_script ---"
  
  # Run the script and capture both stdout and stderr
  ERROR_OUTPUT=$(Rscript "$r_script" 2>&1)
  EXIT_CODE=$?
  
  # Treat any non-zero exit code as failure, including display/viewer errors
  if [ $EXIT_CODE -eq 0 ]; then
    echo "--- Successfully executed: $r_script ---"
    echo "[SUCCESS] $r_script" >> "$OUTPUT_FILE"
    ((SUCCESS_COUNT++))
  else
    echo "--- Failed to execute: $r_script ---"
    echo "[FAILED] $r_script" >> "$OUTPUT_FILE"
    echo "  Exit code: $EXIT_CODE" >> "$OUTPUT_FILE"
    echo "  Error message:" >> "$OUTPUT_FILE"
    printf '%s\n' "$ERROR_OUTPUT" | sed 's/^/    /' >> "$OUTPUT_FILE"
    echo "" >> "$OUTPUT_FILE"
    ((FAIL_COUNT++))
  fi
done <<< "$scripts"

# Clean up
rm -r tinytable_assets 2>/dev/null
rm -r Rplots.pdf 2>/dev/null

# Write summary to output file
echo "" >> "$OUTPUT_FILE"
echo "========================================" >> "$OUTPUT_FILE"
echo "SUMMARY" >> "$OUTPUT_FILE"
echo "Successful: $SUCCESS_COUNT" >> "$OUTPUT_FILE"
echo "Failed: $FAIL_COUNT" >> "$OUTPUT_FILE"
echo "Completed at: $(date)" >> "$OUTPUT_FILE"

echo "Finished execution of all R scripts."
echo "Results written to: $OUTPUT_FILE"

if [ "$FAIL_COUNT" -gt 0 ]; then
  echo "One or more R scripts failed. See $OUTPUT_FILE for details." >&2
  exit 1
fi
