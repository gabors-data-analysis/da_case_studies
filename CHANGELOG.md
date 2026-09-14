# Changelog


## 1.0.0 "Of Course I Still Love You" — 14 September 2026

Substantial update since v.0.9.0: move to `uv` for Python environment, all Stata files updated for version 18. GitHub Codespaces integration. 

### Highlights

- Python moved from conda to [uv](https://docs.astral.sh/uv/), with an exact lockfile.
- R dependencies are pinned with `renv` at the repository root, on R 4.5.2.
- Continuous integration runs all 52 notebooks and 50 of the 54 R scripts on three operating systems.
- GitHub Codespaces gives a working Python or R environment in the browser, one click, nothing to install.
- All 45 Stata do-files rewritten for Stata 18, and every one of them run against the data.
- The long-running machine learning chapters ship with pre-trained models, so they finish in minutes instead of hours.
- 230 commits since v0.9.0.


### Environments and reproducibility

**Python.** The three conda environment files (`daenv_linux.yml`, `daenv_macos.yml`, `daenv_windows.yml`) and `requirements.txt` are gone, replaced by `pyproject.toml`, `uv.lock` and `.python-version`. One lockfile now covers macOS on Apple silicon and Intel, Linux, and Windows, so the four platforms resolve to the same package versions instead of drifting apart. Python is 3.12.4. `numpy` is held at 1.26.4 and `xgboost` at 3.1.3 on every platform; `prophet` moved to 1.2.1 after a long fight with its Stan backend on macOS.

**R.** `renv.lock` moved from `ch00-tech-prep/` to the repository root, so an RStudio project opened at the root picks it up. It pins 280 packages against R 4.5.2. `ch00_install_libraries.R` has been deleted — `renv::restore()` replaces it. We checked every `library()` and `::` call in the R code against the lockfile: 69 packages are loaded, 67 are pinned, and the two that are not (`grid`, `parallel`) ship with R itself.

**Stata.** No dependency management, as before. `ch00_install_libraries.do` still installs the user-written commands.

### Continuous integration

Four workflows, all new:

| Workflow | What it does |
| --- | --- |
| `test_notebooks.yml` | Converts and executes notebooks on Ubuntu, macOS and Windows |
| `test_r_scripts.yml` | Executes R scripts on the same three |
| `build_py_env.yml` | Builds and pushes the Python Docker image to `ghcr.io/gabors-data-analysis/da_case_studies/py_env` |
| `build_r_env.yml` | Same for `r_env` |

A push runs only the chapters it touched; changing `uv.lock`, `renv.lock` or the test harness runs everything. The full suite on the last commit before this release executed all 52 notebooks and 50 of the 54 R scripts on each of three operating systems, with no failures.

Four R scripts are held out of the automated run because they take too long for a hosted runner: the random forest in `ch16`, the firm exit prediction in `ch17`, and the two smoking health risk scripts in `ch11`.

Stata is not tested automatically. GitHub's runners have no Stata licence.

### GitHub Codespaces

`.devcontainer/` now holds a Python and an R configuration, each pointing at a Docker image built by CI from the same lockfiles the tests use. The README has a button for each. `.devcontainer/scripts/download-data.sh` pulls the data repository from OSF and puts it where the code expects.

This removes the first session of every course that used to go on installing things.

### Python

- **Pre-trained models for the slow chapters.** `ch16` and `ch17` now ship their fitted models as compressed pickles (50 MB in total, in `pickled_models/`). A `TRAIN_MODELS_FLAG` at the top of the notebook decides: left at `"No"`, the notebook loads them and runs in minutes; set to `"Yes"`, it fits everything from scratch as before. This is what let those two chapters join the test suite at all.
- **Model interpretation reworked** in `ch16`. LIME now comes before SHAP, which reads better as a sequence. Added ICE curves, a small table of LIME predictions, a sensitivity check for SHAP, and XGBoost to the model horserace.
- Variable importance code refactored into reusable pieces (issue #141).
- `py_helper_functions.py` renamed to `da_helper_functions.py`, matching `da_helper_functions.R`. **If you import it in your own code, update the name.**
- `test_env.py` became `ch00-tech-prep/tests/run_all_python.py`. It takes chapter folders as arguments to check one case study at a time.
- All 52 notebooks now declare the same kernel, `da-case-studies (3.12.4)`.
- Notebooks are exported as UTF-8, and non-decodable characters removed, which was breaking the Windows runs.
- Duplicate `ch10-gender-earnings-multireg-pyfixest.ipynb` deleted; the main notebook already uses pyfixest.
- `n_jobs` set to 1 on model objects that were nesting parallelism inside a parallel cross-validation.

### R

- `ch16` gained parallel processing, permutation importance, explainability output and XGBoost, bringing it closer to the Python version.
- **`ch14-airbnb-prepare` in R and Python now produce the same `airbnb_london_workfile.csv`.** They had quietly diverged.
- ggplot2 deprecation warnings cleared across the codebase, and the long-standing `geom_smooth()` warning fixed (issue #10).
- Unused `library()` calls removed from many scripts, and `geom_line_da` parameters tidied.
- Fixed the data input path for the Case-Shiller workfile in the home prices script, and the `ch21` path.
- Dropped the `glance()` call from the logit summary in the smoking health risk analysis.
- `ch24` R, Python and Stata files renamed with the `ch24-` prefix the other chapters use.

### Stata

All 45 do-files are now Stata 18 code. They declare `version 18`, set `set varabbrev off`, drop command abbreviations, and use current graph colour syntax. Chapters 1–12 were converted in December 2025; chapters 13–24 followed in this release (PR #149).

**This is a breaking change.** `version 18` makes Stata 17 and below stop rather than run. If you are on an older Stata, the v0.9.0 tag has the previous files.

The second half of the conversion turned up real bugs, not just version syntax:

- **`table x, c(...)` was removed in Stata 17.** Eight call sites in ch13, ch14 and ch18 errored with `r(198)`; rewritten to `statistic()`.
- **Three user-written commands were never installed by `ch00_install_libraries.do`** — `psmatch2` (ch21), `synth` (ch24-haiti) and `heatplot` (ch18-swimmingpool). All three chapters stopped with `r(199)` on a fresh machine. Added. ch18 even carried a comment telling the reader to install `heatplot` by hand.
- **63 abbreviated variable names across ten files** only resolved because variable abbreviation was on. Spelled out. The sharpest case: figure 24.2a plotted `_Y_synth`, which is `synth`'s `_Y_synthetic`. (`synth` itself needs abbreviation on, so that one call is wrapped.)
- **Four data paths no longer resolved.** ch18-case-shiller read a `.dta` the data repository does not ship — it now reads the same `.csv` the R code does; ch24-football read underscores where the shipped `.dta` is hyphenated; ch19_food-health-maker had a hardcoded relative path and no header at all; ch18-swimmingpool now falls back to OSF for its 180 MB raw file.
- **A loop bug in ch13**: the Table 13.2 export passed ``ctitle(Model `v')`` inside a `forval i=` loop, so every column title came out blank.

Chapters 1–12 also claimed in their headers that "Backward compatibility notes for Stata 15 and below are included". No file contained any such notes, and `version 18` stops an older Stata before it could reach them. Removed from all 28; the version note now reads the same in all 45.

Also removed the `ch21` propensity-score matching scratch file, a work file that should not have been committed.

**Verification.** All 45 do-files were run on Stata 18.0 MP against a full `da_data_repo`. 43 finish with `rc=0`. Two run without a single error but were not taken to the last line, because both end in very long simulations: `ch05-stock-market-loss-generalize` does a 10,000-iteration bootstrap that re-reads and re-saves a file on every pass, and `ch14-airbnb-prediction` closes with a `cvlasso` 5-fold cross-validation on the full interaction model. Both are pre-existing runtimes, not regressions.

### Documentation

- New [Codespaces guide](ch00-tech-prep/da-setup-codespaces.md).
- The [Python setup guide](ch00-tech-prep/da-setup-python.md) rewritten for uv.
- The [R setup guide](ch00-tech-prep/da-setup-r.md) updated from R 4.0.5 to 4.5.2 and the `renv` workflow.
- The [Stata setup guide](ch00-tech-prep/da-setup-stata.md) rewritten: Stata 18 throughout, and a note on the two case studies that need extra data.
- Added `ch00-tech-prep/set-data-directory-example.do`. Every do-file and the setup guide referred to it, but it had never been committed.
- Added `ch00-tech-prep/set-data-directory-example.R`.
- READMEs added for `preprocessing/` and `preprocessing/cps-earnings/`.
- Screenshots in `ch00-tech-prep/pics/` renamed from numbers to descriptions, and nine unused ones deleted.
- OSF download URLs updated to the new format; the old ones no longer resolved.

### Known limitations

- Stata is not covered by continuous integration, because GitHub's runners have no Stata licence. All 45 do-files were run by hand on Stata 18.0 MP for this release, but nothing stops them drifting again.
- Two Stata case studies are very slow. `ch05-stock-market-loss-generalize` bootstraps 10,000 times with a disk round-trip per iteration; `ch14-airbnb-prediction` ends in a `cvlasso` cross-validation over hundreds of interaction terms. Both run correctly; both take hours. Worth rewriting in a later release.
- `fct_explicit_na()` is called 19 times across `ch13`, `ch14`, `ch15` and `ch16`. `forcats` deprecated it in 1.0.0. It still runs under the pinned `forcats` 1.0.1, with a warning, so this is tidiness rather than breakage. `fct_na_value_to_level()` is the replacement.
- The saved output of `ch18-swimmingpool-predict.ipynb` still shows conda paths from the machine it was last run on. Cosmetic; it clears the next time the notebook is executed and saved.
- The repository now carries 50 MB of pickled models. They are guaranteed to load only in the environment `uv.lock` describes. Set `TRAIN_MODELS_FLAG = "Yes"` if you are on anything else.

---

## 0.9.0 "Frank Exchange of Views" — 14 August 2025

How the repository evolved for each language between v0.8.3 (25 November 2022) and v0.9.0. The move to `seaborn` and `pyfixest` drove most of the Python-side evolution, while R adopted `fixest`. Stata stayed largely as it was.

### Python

- **Seaborn as the plotting backbone.** All chapters migrated from `plotnine` to `seaborn`, with a custom `da_theme`, functions for time-series plots (`tsplots`), and standard figure sizes. This removed the `plotnine` dependency.
- **Regression engine upgrade.** Examples moved from `statsmodels` to `pyfixest`, with formulas rewritten to match the textbook notation, ending on `pyfixest` 0.30.2.
- **New model-interpretation tools.** A LIME explainer, plus helpers for variable importance and splines.
- **Environment clean-up.** New conda YAML files per platform, Python 3.12, and removal of `plotnine` and `shap`.
- **Testing.** First scripts to automate environment creation and notebook testing. OSF paths integrated and data-loading paths standardised.

### R

- **Adoption of fixest and marginaleffects.** Base R `lm()` rewritten to `feols()`; `marginaleffects` added for marginal effects.
- **SHAP support (experimental).** An early experiment on SHAP values for R models, kept for reference.
- General bug fixes and readability work.

### Stata

- Minimal change. A few scripts updated for labels or path handling, and OSF links brought in line.

---

## Earlier releases

| Version | Name | Date |
| --- | --- | --- |
| 0.8.3 | Ethics Gradient | 25 November 2022 |
| 0.8.2 | Very Little Gravitas Indeed | 21 March 2022 |
| 0.8.1 | Sweet and Full of Grace | 22 October 2021 |
| 0.8.0 | What Are The Civilian Applications? | 15 July 2021 |
| 0.7.2 | Limiting Factor | 6 May 2021 |
| 0.7.1 | Little Rascal | 9 March 2021 |
| 0.7.0 | Clear Air Turbulence | 8 January 2021 |
| 0.6.0 | Nervous Energy | 21 September 2020 |
