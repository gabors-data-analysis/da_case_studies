# How to set up your computer for R

---

## Get R

1. Download R 4.5.2.

   R is constantly evolving software, with new versions released regularly. To keep the case studies reproducible, we pin the version used for this codebase, together with the exact version of every package, in the `renv.lock` file in the root of the repository. That lockfile records R 4.5.2.

   Get it from CRAN:

   - For Windows: [R 4.5.2 for Windows](https://cran.r-project.org/bin/windows/base/)
   - For macOS: [R 4.5.2 for macOS](https://cran.r-project.org/bin/macosx/) (pick the Apple silicon or Intel build to match your machine)
   - For Linux: [R for Linux](https://cran.r-project.org/bin/linux/)

   A newer R will usually work, but we only test on 4.5.2. On Windows you will also want [Rtools](https://cran.r-project.org/bin/windows/Rtools/) so that packages needing compilation can build.

2. We suggest using RStudio as the editor for R code. (There are many other options, too.)
   You can get [RStudio](https://posit.co/download/rstudio-desktop/) for free.

   If you would rather not install anything, you can run the R case studies in the browser instead — see [how to use GitHub Codespaces](da-setup-codespaces.md).

---

## How to run case studies in R

### 1. Set the working directory for your project

In case you use `RStudio`, create a new `RStudio` project in the root of the `da_case_studies` folder and load it every time you are working on the project.
See the [official documentation](https://support.posit.co/hc/en-us/articles/200526207-Using-RStudio-Projects) on how to create and use `RStudio` projects.

The project must sit in the repository root, because that is where `renv.lock` lives and where the code expects to find the chapter folders.

---

### 2. Install required packages

We use `renv` for dependency management. Open the R project you created in Step 1, and install `renv` by running the following command in the RStudio console:

```r
install.packages("renv")
```

Then install all the packages and dependencies used in the case studies stored in the renv.lock file:

```r
renv::restore()
```

This takes a while the first time — it installs a few hundred packages at the exact versions we tested against. Afterwards, `renv` keeps them in a project-local library, so it will not disturb the R packages you use for other work.

---

### 3. Set project path

You will need to set the path to the data repo and save it in the
`set-data-directory.R` file. Open `set-data-directory-example.R` and add your path to the data repo where you have or will download datasets.
Save as `set-data-directory.R` (exactly where you found `set-data-directory-example.R`).

---

### 4. Check that it works

From the repository root you can run a single case study end to end:

```bash
bash ch00-tech-prep/tests/run_all_r.sh ch13-used-cars-reg
```

Leave the folder off to run every R script. This is the same check that runs automatically on Linux, macOS and Windows whenever the repository changes.

Four scripts are left out of the automated run because they take very long: the random forest in `ch16`, the firm exit prediction in `ch17`, and the two smoking health risk scripts in `ch11`.
