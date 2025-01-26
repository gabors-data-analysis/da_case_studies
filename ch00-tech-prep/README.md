# Tech README

This includes technical files for R, Stata and Python codes used in case studies.

## Python
Run these commands from terminal/PowerShell to install the Anaconda environment after installing [Anaconda](https://www.anaconda.com/download):

V1

```
conda env create -f daenv.yml
jupyter lab
```

V2

```
conda create -n da_env python=3.12.4
conda activate da_env
pip install -r ch00-tech-prep/requirements.txt
jupyter lab
```

## R: ch00_install_libraries.R
* This file was written to install libraries  Using R 4.0.2, version 0.8 2020-09-07
* We suggest using renv instead
* kept it for continuity

## Stata: ch00_install_libraries
You just need to run this once. 
