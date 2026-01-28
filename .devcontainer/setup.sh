#!/bin/bash
set -e

echo "========================================="
echo "Setting up Data Analysis environment..."
echo "========================================="

# Store the workspace folder path
WORKSPACE_FOLDER="${CODESPACE_VSCODE_FOLDER:-$(pwd)}"

# -----------------------------
# Ask user what to install
# -----------------------------
echo ""
echo "What environment(s) would you like to install?"
echo "  1) Python"
echo "  2) R"
echo "  3) Both Python and R"
read -p "Enter choice [1-3]: " ENV_CHOICE

echo ""
read -p "Do you want to download the data repository? [y/n]: " DOWNLOAD_DATA

INSTALL_PYTHON=false
INSTALL_R=false

case "$ENV_CHOICE" in
  1)
    INSTALL_PYTHON=true
    ;;
  2)
    INSTALL_R=true
    ;;
  3)
    INSTALL_PYTHON=true
    INSTALL_R=true
    ;;
  *)
    echo "Invalid choice. Exiting."
    exit 1
    ;;
esac

# -----------------------------
# Conda + Python setup
# -----------------------------
if [ "$INSTALL_PYTHON" = true ]; then
  echo ""
  echo "Updating conda..."
  conda update -n base -c defaults conda -y

  echo "Creating Python environment 'daenv'..."
  cd "${WORKSPACE_FOLDER}/da_case_studies/ch00-tech-prep"

  # Rename example R file (needed regardless if Python-only)
  cp set-data-directory-example.R set-data-directory.R

  conda env create -f daenv_linux.yml

  # Initialize conda for bash
  conda init bash
  source ~/.bashrc

  echo "Python environment 'daenv' created successfully!"
  echo "Activate it with: conda activate daenv"
fi

# -----------------------------
# Data download
# -----------------------------
if [[ "$DOWNLOAD_DATA" =~ ^[Yy]$ ]]; then
  echo ""
  echo "Downloading data files..."
  cd "${WORKSPACE_FOLDER}"
  curl -J -L -O https://osf.io/download/9gw4a

  echo "Unzipping data files..."
  unzip da_data_repo.zip
else
  echo ""
  echo "Skipping data download."
fi

# -----------------------------
# R + renv setup
# -----------------------------
if [ "$INSTALL_R" = true ]; then
  echo ""
  echo "Installing R packages using renv..."
  cd "${WORKSPACE_FOLDER}/da_case_studies"

  echo "Restoring R packages from renv.lock..."
  Rscript -e "
    renv::restore(
      prompt = FALSE,
      repos = c(CRAN = 'https://packagemanager.posit.co/cran/latest')
    )
  "
fi

# -----------------------------
# Final message
# -----------------------------
echo ""
echo "========================================="
echo "Setup complete!"
echo "========================================="

if [ "$INSTALL_PYTHON" = true ]; then
  echo ""
  echo "To use Python:"
  echo "  1. Run: conda activate daenv"
  echo "  2. Open any Jupyter notebook (.ipynb file)"
fi

if [ "$INSTALL_R" = true ]; then
  echo ""
  echo "To use R:"
  echo "  1. Open any R script (.R file)"
  echo "  2. R packages are already installed via renv"
fi

echo ""
echo "========================================="
