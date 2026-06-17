#!/bin/bash

# Get the directory where this script resides
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Set the starting directory to the repository root
START_DIR="$(dirname "$(dirname "$SCRIPT_DIR")")"
cd "$START_DIR" || exit 1

is_r_script() {
  case "$1" in
    *.R|*.r) return 0 ;;
    *) return 1 ;;
  esac
}

is_excluded_script() {
  case "$1" in
    */ch00-tech-prep/*|\
*/renv/*|\
*/ch16-airbnb-random-forest/ch16-airbnb-random-forest.R|\
*/ch17-predicting-firm-exit/ch17-predicting-firm-exit.R|\
*/ch11-smoking-health-risk/ch11-smoking-health-risk-01-munging.R|\
*/ch11-smoking-health-risk/ch11-smoking-health-risk-02-analysis.R)
      return 0
      ;;
    *)
      return 1
      ;;
  esac
}

script_candidates_in_dir() {
  find "$1" -type f \( -name "*.R" -o -name "*.r" \) | while IFS= read -r candidate; do
    if ! is_excluded_script "$candidate"; then
      printf '%s\n' "$candidate"
    fi
  done
}

resolve_target_path() {
  case "$1" in
    /*) printf '%s\n' "$1" ;;
    *) printf '%s/%s\n' "$START_DIR" "$1" ;;
  esac
}

selected_scripts() {
  if [ "$#" -eq 0 ]; then
    script_candidates_in_dir "$START_DIR"
    return
  fi

  for target in "$@"; do
    target_path="$(resolve_target_path "$target")"

    if [ -d "$target_path" ]; then
      script_candidates_in_dir "$target_path"
    elif [ -f "$target_path" ] && is_r_script "$target_path" && ! is_excluded_script "$target_path"; then
      printf '%s\n' "$target_path"
    else
      echo "Skipping non-eligible R target: $target" >&2
    fi
  done
}

order_scripts() {
  awk '
    /(prepare|munging|setup|dataprep|maker)/ { print "0\t" $0; next }
    { print "1\t" $0 }
  ' | sort -k1,1 -k2,2 | cut -f2-
}

# Create output file with timestamp
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
OUTPUT_FILE="$SCRIPT_DIR/r_scripts_results_$TIMESTAMP.log"

if [ "$#" -eq 0 ]; then
  echo "Starting execution of all R scripts in directory: $START_DIR"
else
  echo "Starting execution of selected R scripts in directory: $START_DIR"
fi
echo "Results will be written to: $OUTPUT_FILE"

# Initialize the output file
echo "R Script Execution Results - $(date)" > "$OUTPUT_FILE"
echo "========================================" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"

# Counters for summary
SUCCESS_COUNT=0
FAIL_COUNT=0

# Sort selected files with data-prep style scripts first, then alphabetically.
scripts=$(selected_scripts "$@" | sort -u | order_scripts)

if [ -z "$scripts" ]; then
  echo "No eligible R scripts selected."
  echo "No eligible R scripts selected." >> "$OUTPUT_FILE"
  exit 0
fi

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
