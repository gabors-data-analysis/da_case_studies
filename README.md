# Data Analysis Case Study codebase for R, Python and Stata

**R, Python and Stata code for**  
**Data Analysis for Business, Economics, and Policy**   
by Gábor Békés (CEU) and Gábor Kézdi (U. Michigan)   
Published on 6 May 2021 by Cambridge University Press  
[**gabors-data-analysis.com**](https://gabors-data-analysis.com/)

*Last update: 2026-09-14*

## How to use

All code available for R, Stata and Python. To see options for various languages, check out:

1. **R** --  [How to run code in R ](ch00-tech-prep/da-setup-r.md)
2. **Stata** -- [How to run code in Stata ](ch00-tech-prep/da-setup-stata.md)
3. **Python** -- [How to run code in Python ](ch00-tech-prep/da-setup-python.md)

On the [textbook's website](https://gabors-data-analysis.com/), we have detailed discussion of how to set up libraries, get data: [Overview of data and code](https://gabors-data-analysis.com/data-and-code/)

## GitHub codespaces (NEW)

As of March 2026, you can also run *Python* and *R* codes in [GitHub Codespaces](https://github.com/features/codespaces) with pre-configured environments. You can read more details on how our [case studies work in Codespaces](ch00-tech-prep/da-setup-codespaces.md). 

To start a Codespace for your desired language, press one of the buttons below:

**Click to open Codespaces with *Python* environment:**
[![Open in GitHub Codespaces for Python](https://github.com/codespaces/badge.svg)](https://codespaces.new/gabors-data-analysis/da_case_studies?quickstart=1&devcontainer_path=.devcontainer%2Fpython%2Fdevcontainer.json)

**Click to open Codespaces with *R* environment:**
[![Open in GitHub Codespaces for R](https://github.com/codespaces/badge.svg)](https://codespaces.new/gabors-data-analysis/da_case_studies?quickstart=1&devcontainer_path=.devcontainer%2Fr%2Fdevcontainer.json)

This is a new feature, if you find a bug or have ideas, post an issue or a PR. Or contact us.  

## Status

[![Test Notebooks](https://github.com/gabors-data-analysis/da_case_studies/actions/workflows/test_notebooks.yml/badge.svg)](https://github.com/gabors-data-analysis/da_case_studies/actions/workflows/test_notebooks.yml)
[![Test R Scripts](https://github.com/gabors-data-analysis/da_case_studies/actions/workflows/test_r_scripts.yml/badge.svg)](https://github.com/gabors-data-analysis/da_case_studies/actions/workflows/test_r_scripts.yml)

The [latest release, 1.0.0 "Of Course I Still Love You"](https://github.com/gabors-data-analysis/da_case_studies/releases/tag/v1.0.0) was released 14 September 2026. See the [changelog](CHANGELOG.md) for details.

This is the first release we call finished rather than pre-release. Every Python notebook and every R script now runs end to end in continuous integration on Linux, macOS and Windows, from a locked environment: `uv.lock` for Python, `renv.lock` for R. You can also run the whole thing in the browser through GitHub Codespaces.

All 45 Stata do-files have been rewritten for Stata 18 and run against the data by hand — Stata cannot go in continuous integration, because GitHub's runners have no licence. **This means Stata 17 and below will no longer run the code.** See [how to run code in Stata](ch00-tech-prep/da-setup-stata.md). No Julia yet.

## Organization
1. Each case study has a separate folder.
2. Within case study folders, codes in different languages are simply stored together. 
3. Data should be downloaded and stored in a separate folder. 

## Code language versions
1. **R** -- We use R 4.5.2, with packages pinned in `renv.lock`.
2. **Stata** -- We use version 18. Every `.do` file declares `version 18`, so older Stata will stop rather than run.
3. **Python** -- We use Python 3.12.4, managed with [uv](https://docs.astral.sh/uv/).

## Testing the code

Every notebook and R script is run on each push, on Linux, macOS and Windows. You can run the same checks yourself from the repository root:

```bash
uv run python ch00-tech-prep/tests/run_all_python.py
```

```bash
bash ch00-tech-prep/tests/run_all_r.sh
```

Both accept a chapter folder to check just one case study, for example `bash ch00-tech-prep/tests/run_all_r.sh ch13-used-cars-reg`. They need the data repository downloaded and the data directory set.

## Get data
Data is hosted on OSF.io

[Get data by datasets](https://osf.io/7epdj/)  

## Found an error or have a suggestion?
Awesome, we know there are errors and bugs. Or just much better ways to do a procedure.

To make a suggestion, please open a `github issue` here with a title containing the case study name. You may also contact [us directctly](https://gabors-data-analysis.com/contact-us/). Cheers!
