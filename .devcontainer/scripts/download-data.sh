#!/usr/bin/env bash
# Download and extract data for Data Analysis Case Studies
#
# Usage: bash .devcontainer/scripts/download-data.sh
# Safe to run multiple times.

set -euo pipefail

echo "========================================="
echo "Data Download Script"
echo "========================================="

# Workspace root (Codespaces or local)
WORKSPACE_FOLDER="${CODESPACE_VSCODE_FOLDER:-$(pwd)}"

DATA_DIR="/workspaces/da_data_repo"
DATA_ARCHIVE="${WORKSPACE_FOLDER}/da_data_repo.zip"
DATA_URL="https://osf.io/download/9gw4a"

# Exit early if data already exists
if [[ -d "$DATA_DIR" ]]; then
  echo "Data directory already exists at:"
  echo "  $DATA_DIR"
  echo "Skipping download."
  exit 0
fi

echo ""
echo "Downloading data from OSF repository..."
echo "URL: $DATA_URL"

if command -v curl >/dev/null 2>&1; then
  curl -fL -o "$DATA_ARCHIVE" "$DATA_URL"
elif command -v wget >/dev/null 2>&1; then
  wget -O "$DATA_ARCHIVE" "$DATA_URL"
else
  echo "Error: curl or wget is required but not installed."
  exit 1
fi

echo ""
echo "Extracting archive..."
unzip -q "$DATA_ARCHIVE" -d "$WORKSPACE_FOLDER" -x "__MACOSX/*"

echo "Cleaning up..."
rm -f "$DATA_ARCHIVE"

echo "Moving data to final location..."
mv "${WORKSPACE_FOLDER}/da_data_repo" "/workspaces/"

echo ""
echo "========================================="
echo "Data download complete!"
echo "========================================="
echo "Data available at:"
echo "  /workspaces/da_data_repo"
echo ""
