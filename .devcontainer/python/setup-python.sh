#!/bin/bash
# Python environment setup script for Data Analysis Case Studies
# This script is executed non-interactively during devcontainer creation

set -e  # Exit on any error

echo "========================================="
echo "Setting up Python environment..."
echo "========================================="

# Update conda to latest version
echo "Updating conda..."
conda update -n base -c defaults conda -y

# Navigate to the environment definition
cd /workspaces/da_case_studies/ch00-tech-prep

# Create the example R data directory file (needed by some notebooks)
# This is done regardless of environment choice
if [ ! -f "set-data-directory.R" ]; then
  cp set-data-directory-example.R set-data-directory.R
fi

# Create the Python environment from the conda environment file
echo "Creating Python environment 'daenv' from daenv_linux.yml..."
conda env create -f daenv_linux.yml

# Initialize conda for bash shell
echo "Initializing conda for bash..."
conda init bash

echo ""
echo "========================================="
echo "Python environment setup complete!"
echo "========================================="
echo ""
echo "The 'daenv' conda environment is ready."
echo "To activate it manually, run: conda activate daenv"
echo ""
echo "Note: Data files are NOT downloaded automatically."
echo "To download data, run: bash .devcontainer/scripts/download-data.sh"
echo ""
