# How to set up your computer for R

---

## Get R

1. Download R 4.0.5.

   R is constantly evolving software, with new versions being made available regularly.  
   To make sure all codes are reproducible, we set version 4.0.5 as the version used for this textbook.

   To start working in R, please download version 4.0.5 from below. After download, double-click the file and follow the instructions. (If you are using a newer version of R, codes shall still work, but there is no guarantee.)

   - For Windows [R 4.0.5 for Windows](https://cran.r-project.org/bin/windows/base/old/4.0.5/)
   - For Mac [R 4.0.5 for Mac](https://cran.r-project.org/bin/macosx/base/R-4.0.5.pkg) and [R 4.1.0 for M1](https://cran.r-project.org/bin/macosx/big-sur-arm64/base/)

2. We suggest using R Studio as the editor for R code. (There are many other options, too.)  
   You can get [R Studio](https://rstudio.com/products/rstudio/download/) for free.

---

## How to run case studies in R

### 1. Set the working directory for your project

In case you use `RStudio`, create a new `RStudio` project for the case studies and load it every time you are working on the project.  
See the [official documentation](https://support.posit.co/hc/en-us/articles/200526207-Using-RStudio-Projects) on how to create and use `RStudio` projects.

---

### 2. Install required packages

We use `renv` for dependency management. Open the R project you created in Step 1, and install `renv` by running the following command in the R studio console:

```r
install.packages("renv")
```

Then install all the packages and dependencies used in the case studies stored in the renv.lock file:


```r
renv::restore()
```

### 3. Set project path
You will need to set the path to the data repo and save it in the
`set-data-directory.R` file. Open` set-data-directory-example.R` and add your path to the data repo where you have or will download datasets.
Save as `set-data-directory.R` (exactly where you found `set-data-directory-example.R`).
