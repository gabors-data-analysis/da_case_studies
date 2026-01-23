# GitHub Codespaces Setup for Data Analysis Case Studies

This directory contains the configuration for GitHub Codespaces, enabling you to get started with the Data Analysis case studies without having to install Python, R, or any dependencies on your local machine.

## What is GitHub Codespaces?

GitHub Codespaces provides a complete, ready-to-use development environment in the cloud. It includes:
- Python 3.12 with all required packages
- R 4.4 with all required packages
- VS Code editor in your browser
- All tools and extensions pre-configured

## How to Use Codespaces

### Starting a Codespace

1. Navigate to the [repository on GitHub](https://github.com/khxu/da_case_studies)
2. Click the green "Code" button
3. Select the "Codespaces" tab
4. Click "Create codespace on main" (or your desired branch)

Your Codespace will be created and set up automatically. The initial setup takes about 5-10 minutes as it installs all Python and R packages.

### Using Python in Codespaces

1. Once your Codespace is ready, open a terminal
2. Activate the Python environment:
   ```bash
   conda activate daenv
   ```
3. Open any Jupyter notebook (`.ipynb` file) or Python script
4. Select the `daenv` kernel/interpreter when prompted

### Using R in Codespaces

1. Open any R script (`.R` file)
2. R packages are already installed and ready to use via `renv`
3. The R extension for VS Code is pre-configured

### Getting the Data

The code requires data files that are hosted separately on OSF.io:

**Option 1 - All datasets (recommended for offline work):**
- Download all datasets at once from [OSF](https://osf.io/3u5em/files/osfstorage)
- Create a `da_data_repo` folder in your workspace
- Extract all data files there

**Option 2 - Specific datasets (lighter download):**
- Download only the datasets you need from [OSF](https://osf.io/7epdj/)
- Create a `da_data_repo` folder in your workspace
- Place the downloaded datasets there

### Customizing Your Environment

If you need to install additional packages:

**For Python:**
```bash
conda activate daenv
pip install package-name
```

**For R:**
```R
install.packages("package-name")
```

## What's Included

### Python Environment
- Python 3.12.4
- All packages from `ch00-tech-prep/daenv_linux.yml`
- Key libraries: pandas, numpy, scikit-learn, matplotlib, seaborn, statsmodels, prophet, and more

### R Environment  
- R 4.4
- All packages from `renv.lock`
- Key libraries: tidyverse, fixest, modelsummary, ggplot2, caret, and more

### VS Code Extensions
- Python support (IntelliSense, debugging, formatting)
- Jupyter notebook support
- R language support and debugging
- Git integration

## Troubleshooting

**If Python packages are missing:**
```bash
cd /workspaces/da_case_studies/ch00-tech-prep
conda env create -f daenv_linux.yml --force
```

**If R packages are missing:**
```R
renv::restore()
```

**To rebuild the entire environment:**
Click the "Rebuild Container" option from the Command Palette (Ctrl+Shift+P or Cmd+Shift+P)

## Cost and Limits

GitHub Codespaces is free for:
- 60 hours per month for personal accounts
- 120 core-hours per month

After that, you may incur charges. See [GitHub's pricing page](https://github.com/features/codespaces) for details.

## More Information

For more detailed setup instructions and information about the case studies, see:
- [R Setup Guide](../ch00-tech-prep/da-setup-r.md)
- [Python Setup Guide](../ch00-tech-prep/da-setup-python.md)
- [Textbook Website](https://gabors-data-analysis.com/)
