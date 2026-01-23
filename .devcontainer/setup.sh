#!/bin/bash
set -e

echo "========================================="
echo "Setting up Data Analysis environment..."
echo "========================================="

# Update conda
echo "Updating conda..."
conda update -n base -c defaults conda -y

# Create Python environment from the Linux conda environment file
echo "Creating Python environment 'daenv'..."
cd "${CODESPACE_VSCODE_FOLDER:-$(pwd)}/ch00-tech-prep"
conda env create -f daenv_linux.yml

# Initialize conda for bash
conda init bash
source ~/.bashrc

echo "Python environment 'daenv' created successfully!"
echo "Activate it with: conda activate daenv"

# Install R packages using renv
echo "Installing R packages using renv..."
cd "${CODESPACE_VSCODE_FOLDER:-$(pwd)}"

# Remove existing renv library to ensure clean restore
echo "Cleaning renv library..."
rm -rf renv/library

# Restore R environment using Posit Package Manager for better binary availability
echo "Restoring R packages from renv.lock..."
Rscript -e "
  # Use Posit Public Package Manager for better historical package availability
  renv::restore(
    prompt = FALSE, 
    repos = c(CRAN = 'https://packagemanager.posit.co/cran/latest')
  )
"

echo "========================================="
echo "Setup complete!"
echo "========================================="
echo ""
echo "To use Python:"
echo "  1. Run: conda activate daenv"
echo "  2. Open any Jupyter notebook (.ipynb file)"
echo ""
echo "To use R:"
echo "  1. Open any R script (.R file)"
echo "  2. R packages are already installed via renv"
echo ""
echo "Data files should be downloaded from:"
echo "  https://osf.io/7epdj/"
echo ""
echo "========================================="
