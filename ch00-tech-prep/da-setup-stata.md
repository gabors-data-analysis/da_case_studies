# How to set up your computer for Stata

---

## Get Stata

1. You will need a Stata license to use it. Your institution may have access, check for that.
2. You may ask for a [student license](https://www.stata.com/customer-service/short-term-license/) too.

---

## Code language versions

1. **You need Stata 18.** Every `.do` file opens with `version 18`, so Stata 17 and below will stop with an error rather than run. The code also sets `set varabbrev off`, meaning variables are always spelled out in full.
2. Data files are saved in the Stata 13 format, so they load in any recent Stata.
3. Chapters 15, 16 and 17 — the machine learning chapters — have no Stata code. Use R or Python for those.

If you are stuck on an older Stata, the v0.9.0 tag has the previous versions of these files, which ran on Stata 13 and up. They are not maintained.

---

## Setting up in Stata

1. Create a folder structure as described in [setting up folders](https://gabors-data-analysis.com/data-and-code/), basically having one directory for the codes (`.do` files) and one for data.
2. The first time you use these codes, run
   ```
   ch00-tech-prep/ch00_install_libraries.do
   ```
   from the case study working directory. This installs the user-written commands the textbook uses, including `outreg2`, `estout`, `psmatch2` and `synth`. Without it several chapters stop with "command ... is unrecognized".

---

## How to run case studies in Stata

1. Each `.do` file will ask you to set up your working directory before first running.
You set it and save the code, so you'll only have to do it once.
2. Data directories will have been set up as well. There are two options:

- **Option 1:** run directory-setting do file (**RECOMMENDED**)
  Open our
  ```
  ch00-tech-prep/set-data-directory-example.do
  ```
  change the path to where you store your data files in the `da_data_repo` directory and save it as
  ```
  set-data-directory.do
  ```
  in the repository root. From now on, this will be automatically used in all the `.do` files.

- **Option 2:** You can set the data file every time by simply adding the path to the `da_data_repo` to the .do files. This is useful if you only use a few files.

---

## Two case studies that need extra data

- **ch18-swimmingpool** starts from the raw transaction file, which is about 180 MB and is not part of the `da_data_repo` download. The code falls back to fetching it from OSF automatically. The R and Python versions skip the aggregation and read the pre-aggregated `swim-transactions/clean/swim_work.csv` instead.
- **ch19_food-health-maker.do** writes `food-health.dta` and `food-health.csv` *back into* the data repository. You only need to run it if you want to rebuild that work file; `ch19-food-health.do` reads the copy that already ships with the data.

---

## A note on testing

Unlike the R and Python code, the Stata code is not checked automatically on every change — GitHub's runners have no Stata licence. All 45 do-files were last run by hand on Stata 18.0 MP in September 2026. Bug reports are especially welcome here.
