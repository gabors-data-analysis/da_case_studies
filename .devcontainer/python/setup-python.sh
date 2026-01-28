#!/bin/bash
# Python environment setup script for Data Analysis Case Studies
# Executed non-interactively during devcontainer creation

set -e

echo "========================================="
echo "Setting up Python environment..."
echo "========================================="

# Resolve workspace folder
WORKSPACE_FOLDER="${CODESPACE_VSCODE_FOLDER:-/workspaces}"

# Navigate to the environment definition
cd "$WORKSPACE_FOLDER/da_case_studies/ch00-tech-prep"

# Create the example R data directory file (needed by some notebooks)
if [ ! -f "set-data-directory.R" ]; then
  cp set-data-directory-example.R set-data-directory.R
fi

# Create conda environment if it does not already exist
echo "Ensuring conda environment 'daenv' exists..."
if conda env list | grep -q "^daenv "; then
  echo "Conda environment 'daenv' already exists. Skipping creation."
else
  conda env create -f daenv_linux.yml
fi

echo ""
echo "========================================="
echo "Python environment setup complete!"
echo "========================================="
echo ""
echo "The 'daenv' conda environment is ready."
echo ""
echo "Note: Data files are NOT downloaded automatically."
echo "To download data, run: bash .devcontainer/scripts/download-data.sh"
echo ""
