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
- Base image: `ghcr.io/rocker-org/devcontainer/r-ver:4.5`
- Setup script: `.devcontainer/r/setup-r.sh`
- Restores packages from: `renv.lock`

### Switching Environments

To switch between Python and R environments:
1. Close the current devcontainer
2. Reopen the folder in VS Code
3. Select a different configuration when prompted

Or use the VS Code command palette:
- `Dev Containers: Rebuild and Reopen in Container`
