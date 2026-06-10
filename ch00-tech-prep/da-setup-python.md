# How to set up the Python environment

This guide prepares your computer to run the Python notebooks for the Data
Analysis case studies. The project uses [uv](https://docs.astral.sh/uv/) to
install Python, create an isolated environment, and install the exact package
versions recorded in `uv.lock`.

The commands below work on Windows, macOS, and Linux. If you prefer not to
install anything locally, use the pre-configured Python
[GitHub Codespace](da-setup-codespaces.md).

## What you need

- **uv** manages Python and the project environment.
- **Visual Studio Code** provides the editor and notebook interface.
- **Git** downloads and updates the repository.
- **Graphviz** is a small system program used by decision-tree diagrams.
- **OpenMP** is required by XGBoost on macOS.

The project currently uses Python 3.12.4. You do not need to install Python
separately: uv installs the correct version when the environment is created.

## 1. Install uv

Follow the [official uv installation guide](https://docs.astral.sh/uv/getting-started/installation/).
The standard commands are:

### Windows PowerShell

```powershell
powershell -ExecutionPolicy ByPass -c "irm https://astral.sh/uv/install.ps1 | iex"
```

### macOS and Linux

```bash
curl -LsSf https://astral.sh/uv/install.sh | sh
```

Close and reopen your terminal, then check the installation:

```bash
uv --version
```

This repository is tested with uv 0.11.20.

## 2. Install VS Code

Download [Visual Studio Code](https://code.visualstudio.com/Download), then
install these extensions from its Extensions panel:

- [Python](https://marketplace.visualstudio.com/items?itemName=ms-python.python)
- [Jupyter](https://marketplace.visualstudio.com/items?itemName=ms-toolsai.jupyter)

## 3. Install Git and clone the repository

Install [Git](https://git-scm.com/downloads), then clone the case studies:

```bash
git clone https://github.com/gabors-data-analysis/da_case_studies.git
cd da_case_studies
```

You can also clone the repository through VS Code with
`Clone Git Repository...`.

## 4. Install system dependencies

Some required components are not Python packages, so uv does not install them.
Use the command for your operating system:

### Windows with Chocolatey

```powershell
choco install graphviz
```

### macOS with Homebrew

```bash
brew install graphviz libomp
```

### Ubuntu or Debian Linux

```bash
sudo apt-get update
sudo apt-get install graphviz
```

See the [Graphviz download page](https://graphviz.org/download/) if you do not
use one of these package managers. On macOS, XGBoost also requires the OpenMP
runtime supplied by Homebrew's `libomp` package.

## 5. Create the project environment

From the repository root, run:

```bash
uv sync --frozen
```

uv will:

1. Install Python 3.12.4 if needed.
2. Create an isolated `.venv` directory.
3. Install the exact dependencies from `uv.lock`.

Run project commands without manually activating the environment:

```bash
uv run python --version
```

Activation is optional. If you want an activated terminal, use:

### Windows PowerShell

```powershell
.venv\Scripts\Activate.ps1
```

### macOS and Linux

```bash
source .venv/bin/activate
```

## 6. Select the environment in VS Code

Open the repository folder in VS Code, then open a notebook.

1. Click **Select Kernel** in the top-right corner.
2. Choose **Python Environments**.
3. Select the interpreter inside the repository's `.venv` directory.

The path ends in `.venv/bin/python` on macOS and Linux, or
`.venv\Scripts\python.exe` on Windows.

VS Code normally discovers this environment automatically after `uv sync`.

## 7. Get the data

Data is hosted on OSF. You can either:

1. Download the complete data repository from
   [OSF](https://osf.io/3u5em/files/osfstorage).
2. Download individual datasets from [OSF](https://osf.io/7epdj/).

Unzip or create `da_data_repo` next to `da_case_studies`:

```text
parent-folder/
├── da_case_studies/
└── da_data_repo/
```

You can now run notebook cells in VS Code.

## Updating the environment

For normal use, keep the locked environment unchanged:

```bash
uv sync --frozen
```

Project maintainers should use `uv add`, `uv remove`, or `uv lock` when
dependencies intentionally change, and commit both `pyproject.toml` and
`uv.lock`.

## Legacy Conda setup

Conda is supported as a deprecated fallback for one transition release. The
files `ch00-tech-prep/daenv_linux.yml`, `daenv_macos.yml`, and
`daenv_windows.yml` remain available temporarily:

```bash
cd ch00-tech-prep
conda env create -f daenv_{SYSTEM}.yml
conda activate daenv
```

Replace `{SYSTEM}` with `linux`, `macos`, or `windows`. New installations
should use uv because the Conda definitions and legacy CI workflow will be
removed after the transition release.
