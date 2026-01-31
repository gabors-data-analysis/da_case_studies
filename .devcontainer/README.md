# Devcontainer Configuration

This repository now supports **two separate devcontainer configurations**:

## 🐍 Python Environment

Location: `.devcontainer/python/`

A lightweight Python-only environment with:
- Conda environment (`daenv`) with all data science packages
- Jupyter notebook support
- VS Code Python extensions

**To use:**
1. Open the repository in VS Code
2. When prompted to reopen in container, select **"Data Analysis - Python"**
3. Wait for the environment to build (first time only)
4. The Python environment will be ready to use

## 📊 R Environment

Location: `.devcontainer/r/`

A lightweight R-only environment with:
- R 4.5 with packages from `renv.lock`
- VS Code R extensions (no RStudio)
- Optimized for VS Code terminal usage

**To use:**
1. Open the repository in VS Code
2. When prompted to reopen in container, select **"Data Analysis - R"**
3. Wait for the environment to build (first time only)
4. R packages will be installed automatically via renv

## 🎨 R with RStudio Server

Location: `.devcontainer/r-rstudio/`

A full-featured R environment with:
- R 4.5 with packages from `renv.lock`
- RStudio Server IDE accessible via browser
- VS Code R extensions (optional)
- Auto-forwarded port for seamless access to RStudio

**To use:**
1. Open the repository in VS Code
2. When prompted to reopen in container, select **"Data Analysis - R with RStudio"**
3. Wait for the environment to build (first time only)
4. R packages will be installed automatically via renv
5. RStudio Server will automatically open in your browser when the container is ready
   - If not, access it manually via the forwarded port link (typically `http://localhost:8787`)
   - No authentication required in the dev container

**Benefits:**
- Full RStudio interface for interactive R development
- Integrated console, editor, and plotting panels
- Better for exploratory data analysis
- Familiar environment for R users

## 📦 Data Download

**Neither environment downloads data automatically.**

To download the data repository manually:

```bash
bash .devcontainer/scripts/download-data.sh
```

This script:
- Downloads data from the OSF repository
- Extracts it to the expected location
- Can be run multiple times safely (checks for existing data)

## 🔧 Legacy Configuration

The original combined R+Python devcontainer configuration is in this directory but is **deprecated**.

Use the new separate configurations in `.devcontainer/python/` or `.devcontainer/r/` instead.

## 📝 Technical Details

### Python Setup
- Base image: `mcr.microsoft.com/devcontainers/miniconda:latest`
- Setup script: `.devcontainer/python/setup-python.sh`
- Creates conda env from: `ch00-tech-prep/daenv_linux.yml`

### R Setup
- **R only** (`r/`):
  - Base image: `ghcr.io/rocker-org/devcontainer/r-ver:4.5`
  - Setup script: `.devcontainer/r/setup-r.sh`
  - No RStudio (VS Code terminal only)
  
- **R with RStudio** (`r-rstudio/`):
  - Base image: `ghcr.io/rocker-org/devcontainer/rstudio:4.5`
  - Setup script: `.devcontainer/r-rstudio/setup-r-rstudio.sh`
  - Includes RStudio Server with automatic port forwarding
  
- Restores packages from: `renv.lock`

### Switching Environments

To switch between Python and R environments:
1. Close the current devcontainer
2. Reopen the folder in VS Code
3. Select a different configuration when prompted

Or use the VS Code command palette:
- `Dev Containers: Rebuild and Reopen in Container`
