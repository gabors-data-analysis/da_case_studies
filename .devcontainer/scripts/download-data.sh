#!/bin/bash
# Data download script for Data Analysis Case Studies
# This script downloads and extracts the data repository
# 
# Usage: bash .devcontainer/scripts/download-data.sh
# 
# This script is idempotent - safe to run multiple times.
# It will skip downloading if the data already exists.

set -e  # Exit on any error

echo "========================================="
echo "Data Download Script"
echo "========================================="

# Navigate to workspace root
cd /workspaces/da_case_studies

# Define data directory and archive name
DATA_DIR="da_data_repo"
DATA_ARCHIVE="da_data_repo.zip"
DATA_URL="https://osf.io/download/9gw4a"

# Check if data already exists
if [ -d "$DATA_DIR" ]; then
  echo ""
  echo "Data directory '$DATA_DIR' already exists."
  read -p "Do you want to re-download and overwrite? [y/N]: " -n 1 -r
  echo
  
  if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Skipping download. Existing data will be kept."
    exit 0
  fi
  
  echo "Removing existing data directory..."
  rm -rf "$DATA_DIR"
fi

# Download the data archive
echo ""
echo "Downloading data from OSF repository..."
echo "URL: $DATA_URL"

if command -v curl &> /dev/null; then
  curl -J -L -o "$DATA_ARCHIVE" "$DATA_URL"
elif command -v wget &> /dev/null; then
  wget -O "$DATA_ARCHIVE" "$DATA_URL"
else
  echo "Error: Neither curl nor wget is available. Cannot download data."
  exit 1
fi

# Extract the archive
echo ""
echo "Extracting data archive..."
unzip -q "$DATA_ARCHIVE"

# Clean up the archive file
echo "Cleaning up archive file..."
rm -f "$DATA_ARCHIVE"

echo ""
echo "========================================="
echo "Data download complete!"
echo "========================================="
echo ""
echo "Data has been extracted to: $DATA_DIR/"
echo ""
