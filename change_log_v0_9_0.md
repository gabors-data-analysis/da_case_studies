# Changelog: version 0.9.0. (2025-08-11)

## Evolution of code by language since v0.8.3

Changelog summary -- how the content of the repository evolved for each programming language (Python, R and Stata) between the previous release **v0.8.3** (25 Nov 2022) and the latest release **v0.9.0**. 

Overall, the transition to `seaborn` and `pyfixest` drove most of the **Python**‑side evolution, while the **R** side adopted `fixest`/`marginaleffects`. **Stata** materials remained largely stable, reflecting a focus on modernizing the Python and R components for reproducibility and ease of use. No Julia yet. 


## Python

Python notebooks underwent substantial refactoring and feature additions:

- **Seaborn as the plotting backbone**: All chapters migrated from the old `plotnine` library to `seaborn`. This included developing a custom `da_theme`, adding functions for time‑series plots (`tsplots`), and standardizing default figure sizes. The change eliminated the dependency on `plotnine` and simplified the plotting pipeline.
- **Regression engine upgrade**: Regression examples were moved from `statsmodels` to the `pyfixest` package. Code was refactored to accommodate the new API, and formulas were updated to match the textbook notation. Throughout the migration several minor bug fixes were applied, and later updates bumped `pyfixest` to version 0.30.2.
- **New model‑interpretation tools**: A LIME explainer was introduced to help interpret machine‑learning models. Other helper functions were added to improve variable importance and spline calculations.
- **Environment and dependency clean‑up**: The Python environment was modernized with new conda/macOS/Windows YAML files, support for Python 3.12, and removal of obsolete packages such as `plotnine` and `shap`. `prophet`, `lime` and other dependencies were updated.
- **Testing and reproducibility**: Scripts were added to automate environment creation and test notebooks. OSF paths/links were integrated, and default data‑loading paths were standardized. Numerous notebooks were tidied up, including fixes to bar‑plot axes and cleaning of gender/earnings data.

## R

Changes in R code were more targeted but still significant:

- **Adoption of fixest and marginaleffects**: Examples previously using base R `lm()` were rewritten to use `feols()` from the `fixest` package. The `marginaleffects` package was added for computing marginal effects, and formulas were aligned with the notation in the textbook. Minor adjustments were made to ensure compatibility with `matchit` and `dplyr` syntax.
- **SHAP support (experimental)**: An early experiment added SHAP value calculations for R models; although later the focus shifted back to Python, the code remains available for reference.
- **General maintenance**: A few bug fixes and readability improvements were made across R scripts, but no major structural changes occurred.

## Stata

Stata materials saw minimal changes during this release cycle:

- **Code stability**: Most `.do` files remained unchanged. A small number of scripts were updated to improve labels or path handling; for example, the football‑manager‑success chapter received a minor update to correct a plot option.
- **Consistency with new data paths**: Where necessary, OSF links and standardized data paths were incorporated to ensure that Stata examples work seamlessly across operating systems.

