# da_case_studies
**Code for Case Studies in**

**Data Analysis for Business, Economics, and Policy** by Gábor Békés (CEU) and Gábor Kézdi (U. Michigan) 

Graduate textbook forthcoming (December 2020) by Cambridge University Press

## Code
1. R
2. Stata
3. Python (in preparation)


## Organization
1. Each case study has a separate folder.
2. Within case study folders, codes in different languages are simply stored together. 
3. Some intermediary files (csv, dta, rds) may be saved there, too. 
4. Currently output is not stored here 

## How to run case studies in R

1. Step 1: Set the working directory for your project.

	- Option 1: [Recommended] In case you use `RStudio` create a new `Rstudio` project for the case studies and load it every time you are working on the project. See the [official documentation](https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects) on how to create and use `Rstudio` projects. 
	- Option 2: Make sure some other way that your working directory is the root folder of the case study repository.

2. Step 2: Create an `.Renviron` file in your project folder and set the directory that contains the use case data. See more information in `.Renviron` file [here](https://cran.r-project.org/web/packages/startup/vignettes/startup-intro.html). 
	- Copy (and rename to `.Renviron`) the `.Renviron_sample` file
	- Set `DATA_DIR` to the absolute path of the directory that contains the use case data
	- If you have set up an `Rstudio` project described in Step 1. then just keep the new `.Renviron` file in your project folder. If you do not use `Rstudio` project then make sure some other way that this `.Renviron` file is processed.

## How to run case studies in Stata

(will be added)

## How to run case studies in Python

(will be added)

