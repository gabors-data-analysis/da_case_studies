#!/bin/bash
# R environment with RStudio Server setup script for Data Analysis Case Studies
# This script is executed non-interactively during devcontainer creation

set -e  # Exit on any error

echo "========================================="
echo "Setting up R + RStudio Server environment..."
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

# Configure RStudio Server for devcontainer
echo ""
echo "Configuring RStudio Server..."

# Disable authentication (safe in devcontainer environment)
# The DISABLE_AUTH environment variable is already set in containerEnv

# Ensure RStudio Server is running
sudo systemctl restart rstudio-server

# Wait for RStudio Server to start
echo "Waiting for RStudio Server to start..."
sleep 5

# Check if RStudio Server is running
if sudo systemctl is-active --quiet rstudio-server; then
  echo "✓ RStudio Server is running on port 8787"
else
  echo "⚠ Warning: RStudio Server may not have started properly"
  echo "  It should start when the container is ready"
fi

# Navigate to the workspace root
cd /workspaces/da_case_studies

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
echo "R + RStudio Server setup complete!"
echo "========================================="
echo ""
echo "✓ RStudio Server is running and accessible via the forwarded port (8787)"
echo "✓ R packages have been installed via renv"
echo "✓ You can now access RStudio Server in your browser or through the port forwarding link"
echo ""
echo "Note: Data files are NOT downloaded automatically."
echo "To download data, run: bash .devcontainer/scripts/download-data.sh"
echo ""
