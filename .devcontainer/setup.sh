#!/bin/bash
set -e

echo "========================================="
echo "Setting up Data Analysis environment..."
echo "========================================="

# Store the workspace folder path
WORKSPACE_FOLDER="${CODESPACE_VSCODE_FOLDER:-$(pwd)}"

# Update conda
echo "Updating conda..."
conda update -n base -c defaults conda -y

# Create Python environment from the Linux conda environment file
echo "Creating Python environment 'daenv'..."
cd "${WORKSPACE_FOLDER}/da_case_studies/ch00-tech-prep"
# rename set-data-directory-example.R to set-data-directory.R
cp set-data-directory-example.R set-data-directory.R

conda env create -f daenv_linux.yml

# Initialize conda for bash
conda init bash
source ~/.bashrc

echo "Python environment 'daenv' created successfully!"
echo "Activate it with: conda activate daenv"

# Install R packages using renv
echo "Installing R packages using renv..."
cd "${WORKSPACE_FOLDER}/da_case_studies"

# Restore R environment using Posit Package Manager for better binary availability
echo "Restoring R packages from renv.lock..."
Rscript -e "
  # Use Posit Public Package Manager for better historical package availability
  renv::restore(
    prompt = FALSE, 
    repos = c(CRAN = 'https://packagemanager.posit.co/cran/latest')
  )
"

cd "${WORKSPACE_FOLDER}"
echo "Downloading data files..."
curl -J -L -O https://osf.io/download/9gw4a
echo "Unzipping data files..."
unzip da_data_repo.zip

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
echo "========================================="
