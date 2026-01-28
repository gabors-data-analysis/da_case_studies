#!/bin/bash
# R environment setup script for Data Analysis Case Studies
# This script is executed non-interactively during devcontainer creation

set -e  # Exit on any error

echo "========================================="
echo "Setting up R environment..."
echo "========================================="

# Install system dependencies that might be needed by R packages
echo "Installing system dependencies..."
sudo apt-get update
sudo apt-get install -y \
  cmake \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  libfontconfig1-dev \
  libharfbuzz-dev \
  libfribidi-dev \
  libfreetype6-dev \
  libpng-dev \
  libtiff5-dev \
  libjpeg-dev

# Navigate to the workspace root
cd /workspaces/da_case_studies_fork

# Create the example R data directory file if it doesn't exist
if [ ! -f "ch00-tech-prep/set-data-directory.R" ]; then
  cd ch00-tech-prep
  cp set-data-directory-example.R set-data-directory.R
  cd ..
fi

# Restore R packages from renv.lock
echo ""
echo "Restoring R packages from renv.lock..."
echo "This may take several minutes on first run..."

Rscript -e "
  renv::restore(
    prompt = FALSE,
    repos = c(CRAN = 'https://packagemanager.posit.co/cran/latest')
  )
"

echo ""
echo "========================================="
echo "R environment setup complete!"
echo "========================================="
echo ""
echo "R packages have been installed via renv."
echo "You can now open and run R scripts."
echo ""
echo "Note: Data files are NOT downloaded automatically."
echo "To download data, run: bash .devcontainer/scripts/download-data.sh"
echo ""
