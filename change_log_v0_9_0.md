# Evolution of code by language since v0.8.3

Changelog summary -- how the content of the repository evolved for each programming language (Python, R and Stata) between the previous release **v0.8.3** (25 Nov 2022) and the latest release **v0.9.0**. 

## Python

Python notebooks underwent substantial updates:

* **Plotting migration**: All visualizations moved from `plotnine` to `seaborn`.

  * Introduced a new `da_theme` for consistent styling.
  * Added helper functions such as `tsplots`.
  * Set default figure sizes across notebooks.

* **Regression engine change**: Migrated from `statsmodels` to `pyfixest` for Python regression examples.

  * Updated formulas to match book conventions.
  * Integrated `marginaleffects`-style outputs where possible.
  * Applied several version updates (e.g., `pyfixest` 0.28.0 → 0.30.2).

* **Environment and dependencies**:

  * Added support for Python 3.12 in conda/macOS/Windows environments.
  * Updated packages (`prophet`, `lime`, etc.).
  * Removed `plotnine` and SHAP from environments.
  * Added OSF file path handling.

* **New features**:

  * LIME explainer for model interpretability.
  * Expanded table and figure outputs.

* **Testing and reproducibility**:

  * New scripts for automated environment creation.
  * OSF integration for datasets.
  * Broader automated notebook testing.

* **Bug fixes and refactoring**:

  * Fixed plotting issues (e.g., bar‑plot axes, density plots).
  * Cleaned data handling and removed unused notebooks.

## R

* **Estimation updates**:

  * Adopted `fixest` for regression models instead of base R’s `lm()`.
  * Integrated `marginaleffects` for marginal effect calculations.

* **Feature additions**:

  *  SHAP integration in R notebooks. (May not be final)

* **General**:

  * Minor path and data‑handling improvements.

## Stata

* Mostly unchanged.
* Minor updates to `.do` files for improved plot labels, standardized OSF links, and path handling.
