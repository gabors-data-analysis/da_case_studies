# Data Analysis Case Study codebase for R, Python and Stata

**R, Python and Stata code for**  
**Data Analysis for Business, Economics, and Policy**   
by Gábor Békés (CEU) and Gábor Kézdi (U. Michigan)   
Published on 6 May 2021 by Cambridge University Press  
[**gabors-data-analysis.com**](https://gabors-data-analysis.com/)


## How to use

All code available for R, Stata and Python. To see options for various languages, check out:

1. **R** --  [How to run code in R ](ch00-tech-prep/da-setup-r.md)
2. **Stata** -- [How to run code in Stata ](ch00-tech-prep/da-setup-stata.md)
3. **Python** -- [How to run code in Python ](ch00-tech-prep/da-setup-python.md)

On the [textbook's website](https://gabors-data-analysis.com/), we have detailed discussion of how to set up libraries, get data: [Overview of data and code](https://gabors-data-analysis.com/data-and-code/)

Alternatively, you can also run Python and R codes in GitHub Codespaces with pre-configured environments. To start a Codespace for your desired language, press one of the buttons below:

**Click to open Codespaces with *Python* environment:**
[![Open in GitHub Codespaces for Python](https://github.com/codespaces/badge.svg)](https://codespaces.new/gabors-data-analysis/da_case_studies?quickstart=1&devcontainer_path=.devcontainer%2Fpython%2Fdevcontainer.json)

**Click to open Codespaces with *R* environment:**
[![Open in GitHub Codespaces for R](https://github.com/codespaces/badge.svg)](https://codespaces.new/gabors-data-analysis/da_case_studies?quickstart=1&devcontainer_path=.devcontainer%2Fr%2Fdevcontainer.json)


## Status

The [Latest release, 0.9.0 "Frank Exchange of Views"](https://github.com/gabors-data-analysis/da_case_studies/releases/tag/v0.9.0) was released 14 August 2025. 

Overall, the transition to `seaborn` and `pyfixest` drove most of the **Python**‑side evolution, while the **R** side adopted `fixest`/`marginaleffects`. **Stata** materials remained largely stable, reflecting a focus on modernizing the Python and R components for reproducibility and ease of use. No Julia yet. See detailes in the changelog / release notes. 

## Organization
1. Each case study has a separate folder.
2. Within case study folders, codes in different languages are simply stored together. 
3. Data should be downloaded and stored in a separate folder. 

## Code language versions
1. **R** -- We used R 4.0.2. 
2. **Stata** -- We used version 15, allmost all code should work in version 13 up.
3. **Python** -- We used Python 3.12.0.

## Get data
Data is hosted on OSF.io

[Get data by datasets](https://osf.io/7epdj/)  

## Found an error or have a suggestion?
Awesome, we know there are errors and bugs. Or just much better ways to do a procedure.

To make a suggestion, please open a `github issue` here with a title containing the case study name. You may also contact [us directctly](https://gabors-data-analysis.com/contact-us/). Cheers!
