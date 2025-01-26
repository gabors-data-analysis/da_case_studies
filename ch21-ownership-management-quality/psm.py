"""
This module provides functions to estimate treatment effects using Propensity Score Matching (PSM) with Nearest Neighbor Matching and to calculate bootstrapped standard errors.

Functions:
    estimate_treatment_effects(data, treatment_col, outcome_col, covariates_formula):
        Estimates the Average Treatment Effect on the Treated (ATET), Average Treatment Effect on the Control (ATC), and Average Treatment Effect (ATE) using PSM.

    bootstrap_iteration(data, treatment_col, outcome_col, covariates, seed):
        Performs a single bootstrap iteration to estimate ATET, ATC, and ATE.

    estimate_bootstrap_se(data, treatment_col, outcome_col, covariates, n_bootstraps=100, n_jobs=-1):
        Estimates the standard errors of ATET, ATC, and ATE using bootstrap resampling.

    psmatch(data: pd.DataFrame, treatment_col: str, outcome_col: str, covariates_formula: str, n_bootstraps: int = 100, n_jobs: int = -1) -> tuple[float, float, float, float, float, float]:
        Perform propensity score matching and estimate treatment effects along with their bootstrapped standard errors.

"""

import warnings

import numpy as np
import pandas as pd
from joblib import Parallel, delayed
from patsy import dmatrices
from sklearn.linear_model import LogisticRegression
from sklearn.neighbors import BallTree

# Suppress sklearn warnings
warnings.filterwarnings("ignore", module="sklearn")


def _estimate_treatment_effects(data, treatment_col, outcome_col, covariates_formula):
    """
    Function to estimate ATET, ATC, and ATE using Propensity Score Matching.
    """
    # Estimate propensity scores using logistic regression
    X = dmatrices(covariates_formula, data, return_type="dataframe")[1]
    y = data[treatment_col]
    logit_model = LogisticRegression(
        penalty=None, solver="newton-cholesky", fit_intercept=True
    )
    logit_model.fit(X, y)
    data["pscore"] = logit_model.predict_proba(X)[:, 1]

    # Separate treated and control groups
    treated = data[data[treatment_col] == 1]
    control = data[data[treatment_col] == 0]

    # Nearest Neighbor Matching on Propensity Scores for Treated Group
    tree_control = BallTree(control[["pscore"]].values, metric="euclidean")
    _, indices = tree_control.query(treated[["pscore"]].values, k=1)
    matched_control = control.iloc[indices.flatten()]

    # Compute ATET
    atet = treated[outcome_col].mean() - matched_control[outcome_col].mean()

    # Nearest neighbor search for control group
    tree_treated = BallTree(treated[["pscore"]].values, metric="euclidean")
    _, indices = tree_treated.query(control[["pscore"]].values, k=1)
    matched_treated = treated.iloc[indices.flatten()]

    # Compute ATC
    atc = matched_treated[outcome_col].mean() - control[outcome_col].mean()

    # ATE is the weighted average of ATET and ATC
    ate = (len(treated) * atet + len(control) * atc) / len(data)

    return atet, atc, ate


def _bootstrap_iteration(data, treatment_col, outcome_col, covariates, seed):
    np.random.seed(seed)
    # Bootstrap resampling
    bootstrap_sample = data.sample(n=len(data), replace=True)
    # Estimate ATET, ATC, and ATE on the bootstrap sample
    return _estimate_treatment_effects(
        bootstrap_sample, treatment_col, outcome_col, covariates
    )


def _estimate_bootstrap_se(
    data,
    treatment_col,
    outcome_col,
    covariates,
    n_bootstraps=100,
    n_jobs=-1,
):
    """
    Estimates the Average Treatment Effect (ATE) using Propensity Score Matching (PSM)
    with Nearest Neighbor Matching, and calculates bootstrapped standard errors.
    """

    results = Parallel(n_jobs=n_jobs)(
        delayed(_bootstrap_iteration)(
            data, treatment_col, outcome_col, covariates, np.random.randint(0, 10000)
        )
        for _ in range(n_bootstraps)
    )

    # Separate the results into ATET, ATC, and ATE lists
    atet_bootstrap, atc_bootstrap, ate_bootstrap = zip(*results)

    # Calculate standard errors (standard deviation of bootstrap estimates)
    atet_se = np.std(atet_bootstrap, ddof=1)
    atc_se = np.std(atc_bootstrap, ddof=1)
    ate_se = np.std(ate_bootstrap, ddof=1)

    return atet_se, atc_se, ate_se


def psmatch(
    data: pd.DataFrame,
    treatment_col: str,
    outcome_col: str,
    covariates_formula: str,
    n_bootstraps: int = 100,
    n_jobs: int = -1,
) -> tuple[float, float, float, float, float, float]:
    """
    Perform propensity score matching and estimate treatment effects.

    Parameters
    ----------
    data: pd.DataFrame
        The dataset containing the treatment, outcome, and covariates.
    treatment_col: str
        The name of the column representing the treatment variable.
    outcome_col: str
        The name of the column representing the outcome variable.
    covariates_formula: str
        A formula specifying the covariates for the propensity score model.
    n_bootstraps: Optional[int]
        The number of bootstrap samples to use for standard error estimation. Default is 100.
    n_jobs: Optional[int]
        The number of parallel jobs to run for bootstrap sampling. Default is -1 (use all processors).

    Returns
    -------
    tuple: A tuple containing the following elements:
        - atet (float): The average treatment effect on the treated.
        - atet_se (float): The standard error of the average treatment effect on the treated.
        - atc (float): The average treatment effect on the control.
        - atc_se (float): The standard error of the average treatment effect on the control.
        - ate (float): The average treatment effect.
        - ate_se (float): The standard error of the average treatment effect.
    """
    atet, atc, ate = _estimate_treatment_effects(
        data, treatment_col, outcome_col, covariates_formula
    )
    atet_se, atc_se, ate_se = _estimate_bootstrap_se(
        data, treatment_col, outcome_col, covariates_formula, n_bootstraps, n_jobs
    )
    return atet, atet_se, atc, atc_se, ate, ate_se
