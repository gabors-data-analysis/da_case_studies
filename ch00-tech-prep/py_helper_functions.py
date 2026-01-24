####################################################
# Import packages
####################################################
from typing import List

import numpy as np
import numpy.typing as npt
import pandas as pd
import matplotlib.pyplot as plt

####################################################
# Define global vars
####################################################
color = ["#3a5e8cFF", "#10a53dFF", "#541352FF", "#ffcf20FF", "#2f9aa0FF"]

dark_gray = ".4"
light_gray = ".8"

da_theme = {
    "axes.edgecolor": dark_gray,
    "axes.facecolor": "white",
    "axes.linewidth": 1,
    "axes.spines.bottom": True,
    "axes.spines.left": True,
    "axes.spines.right": True,
    "axes.spines.top": True,
    "font.family": "sans-serif",
    "figure.figsize": (7, 5.5),
    "grid.color": light_gray,
    "grid.linestyle": "-",
    "text.color": "black",
    "xtick.bottom": True,
    "xtick.color": dark_gray,
    "ytick.color": dark_gray,
}


####################################################
# Define helper functions
####################################################
def seq(start: float, stop: float, by: float, round_n=3) -> list:
    """
    Custom function, used for setting the breaks of scales.

       Parameters
    ----------
    start : float
        Start of the breaks.
    stop : float
        End of the breaks.
    by : float
        Steps between breaks.
    round_n: int, default=3
        Decimals to round floats in output.
    """
    epsilon = np.finfo("float").eps

    return [
        round(x, round_n) for x in list(np.arange(start, stop + (by - epsilon), by))
    ]


def skew(l: npt.ArrayLike, round_n=3) -> float:
    return round((np.mean(l) - np.median(l)) / np.std(l), round_n)

def tabulate(series, drop_missing=False):
    table = (
        pd.concat(
            [
                series.value_counts(dropna=drop_missing)
                .sort_index()
                .round(2)
                .rename("Freq."),
                series.value_counts(normalize=True, dropna=drop_missing)
                .sort_index()
                .rename("Perc."),
            ],
            axis=1,
        )
        .assign(Cum=lambda x: x["Perc."].cumsum())
        .round(3)
    )
    return table

import statsmodels.api as sm

def bs_linreg(x, y, size=1, seed=200999):
    """Perform pairs bootstrap for linear regression."""
    # Set up array of indices to sample from
    inds = np.arange(len(x))

    # Initialize samples
    bs_slope_reps = np.empty(size)
    bs_intercept_reps = np.empty(size)
    rng = np.random.default_rng(seed)
    # Take samples
    for i in range(size):
        bs_inds = rng.choice(inds, len(inds), replace=True)
        bs_x, bs_y = sm.add_constant(x[bs_inds]), y[bs_inds]
        bs_slope_reps[i], bs_intercept_reps[i] = (
            sm.regression.linear_model.OLS(bs_y, bs_x).fit().params
        )

    return bs_slope_reps, bs_intercept_reps

def lspline(series: pd.Series, knots: List[float]) -> np.array:
    """Generate a linear spline design matrix for the input series based on knots.

    Parameters
    ----------
    series : pd.Series
        The input series to generate the design matrix for.
    knots : List[float]
        The list of knots to use for the linear spline.

    Returns
    -------
    np.array
        The design matrix for the linear spline."""
    vector = series.values
    columns = []

    for i, knot in enumerate(knots):
        column = np.minimum(vector, knot if i == 0 else knot - knots[i - 1])
        columns.append(column)
        vector = vector - column

    # Add the remainder as the last column
    columns.append(vector)

    # Combine columns into a design matrix
    return np.column_stack(columns)


def plot_loess(data: pd.DataFrame, x: str, y: str, span: float, color: str = color[1]):
    """
    Plots a LOESS (Locally Estimated Scatterplot Smoothing) curve for the given data.
    Parameters:
    data (pd.DataFrame): The input data as a pandas DataFrame.
    x (str): The column name of the independent variable in the DataFrame.
    y (str): The column name of the dependent variable in the DataFrame.
    span (float): The span parameter for the LOESS model, controlling the degree of smoothing.
    """

    from skmisc.loess import loess

    data = data.dropna(subset=[x]).copy(deep=True)

    loess_pred = loess(data[x], data[y], span=span)
    loess_pred.fit()
    x_plot = np.linspace(data[x].min(), data[x].max(), 1000)
    y_plot = loess_pred.predict(x_plot, stderror=False).values
    plt.plot(x_plot, y_plot, color=color, linewidth=2)


def create_calibration_plot(
    data: pd.DataFrame,
    prob_var: str,
    actual_var: str,
    y_lab="Actual event probability",
    n_bins=10,
    breaks=None,
):
    """
    Function to create calibration curve.
    Returns calibration curve on a plot.

       Parameters
    ----------
    data : pd.DataFrame
        Your dataframe, containing the actual outcome and
        the predicted probabilities of that outcome
        by a model.
    prob_var : str
        Name of the variable, containin predicted
        probabilities.
    actual_var : str
        Name of the actual outcome variable.
    y_lab: str
        Label on y axis of the plot.
    n_bins : int, default=10
        The number of bins, you would like to create.
        This is because with many values for the predicted probabilities,
        we may have few observations to look at for each value.
    breaks (optional): list or None
        You can speficy the breaks of the plot.
    """

    if breaks is None:
        breaks = np.around(
            np.linspace(0, (n_bins + 1) / 10, num=n_bins + 1, endpoint=False),
            decimals=1,
        ).tolist()

    data["prob_bin"] = pd.cut(data[prob_var], breaks, right=True, include_lowest=True)

    binned_data = (
        data.groupby("prob_bin")
        .agg(
            mean_prob=(prob_var, "mean"),
            mean_actual=(actual_var, "mean"),
            n=(actual_var, "size"),
        )
        .reset_index()
    )

    sns.lineplot(
        data=binned_data, x="mean_prob", y="mean_actual", color=color[0], linewidth=1.5
    )

    # Scatter plot with transparency
    sns.scatterplot(data=binned_data, x="mean_prob", y="mean_actual", s=30, alpha=1)

    # Adding a diagonal line
    plt.plot([0, 1], [0, 1], color=color[1], linewidth=0.5)

    plt.xlabel("Predicted event probability")
    plt.ylabel(y_lab)
    plt.xlim(0, 1.01)
    plt.ylim(0, 1.01)
    plt.xticks(np.arange(0, 1.1, 0.1))
    plt.yticks(np.arange(0, 1.1, 0.1))
    plt.show()


def poly(x: npt.ArrayLike, degree=1) -> pd.DataFrame:
    """
    Fit polynomial.

    These are non orthogonal factors, but it may not matter if
    we only need this for predictions (without interpreting the
    coefficients) or visualisation.

    Parameters
    ----------
    x : npt.ArrayLike
        Data array.
    degree : int, default=1
        Degree of the polynomial.
    """
    d = {}
    for i in range(degree + 1):
        if i == 1:
            d["x"] = x
        else:
            d[f"x**{i}"] = np.power(x, i)
    return pd.DataFrame(d)


def plot_bar(
    df: pd.DataFrame,
    x: str,
    y: str,
    hue: str,
    ax: plt.Axes,
    errorbar: str = "se",
    legend_title: str | None = None,
):
    """
    Plot interaction between two variables using a bar plot.
    """
    sns.barplot(
        x=x,
        y=y,
        hue=hue,
        data=df,
        ax=ax,
        errorbar=errorbar,
        capsize=0.1,
        err_kws={"linewidth": 2},
    )

    ax.set_ylabel("Mean Price")
    ax.set_title(None)
    if legend_title:
        ax.legend(title=legend_title)

    ax.set_xlabel("")
    sns.despine(ax=ax, left=True, bottom=True)
    ax.tick_params(left=False)
    ax.yaxis.labelpad = 10


def plot_interactions(df, plot_config):
    """
    Plot for interactions between different variables.

    Args:
        df (pd.DataFrame): The input DataFrame containing the data.
        plot_config (list of dict): List containing plot configurations. Each dict contains the necessary columns and plot settings.
    """
    fig, axes = plt.subplots(3, 2, figsize=(10, 15))

    for idx, config in enumerate(plot_config):
        row = idx // 2
        col = idx % 2
        plot_bar(
            df,
            x=config["x"],
            y=config["y"],
            hue=config["hue"],
            ax=axes[row, col],
            errorbar=config.get("errorbar", "se"),
            legend_title=config.get("legend_title", ""),
        )

    plt.show()


import statsmodels.formula.api as smf
from sklearn.model_selection import KFold
from statsmodels.tools.eval_measures import rmse


def ols_crossvalidator(
    formula: str, data: pd.DataFrame, n_folds=5, average_rmse=True
) -> dict:
    """OLS cross-validator


    Estimates `formula` equation with OLS and returns values of RMSE, R`2, No. coefficients,
    BIC on `data`. Does k-fold cross-validation and either returns train and test RMSE for each
    fold, or return averarage train and test RMSEs.

    Parameters
    ----------
    formula : str
        Equation that is estimated by OLS.
    data : pd.DataFrame
        Database in a wide format.
    n_folds : int, default=5
        Number of folds. Must be at least 2.
    average_rmse : bool, default=True
        Whether to return the average train and test RMSE of the k-fold CV, or return
        train and test RMSE-s for each fold.

    """

    # Get dependent variable

    y = formula.split("~")[0].strip()

    # Get statistics on the whole work data

    model = smf.ols(formula, data=data).fit()

    rsquared = model.rsquared
    # n_coefficients = model.params.shape[0]
    n_coefficients = (
        model.df_model + 1
    )  # This might differ from model.params.shape[0], because of collinear predictors
    bic = model.bic
    rmse_alldata = rmse(model.predict(), data[y])

    # Calculating test and train RMSE-s for each fold

    k = KFold(n_splits=n_folds, shuffle=False, random_state=None)

    rmse_train = []
    rmse_test = []

    for train_index, test_index in k.split(data):

        data_train, data_test = data.iloc[train_index, :], data.iloc[test_index, :]

        model = smf.ols(formula, data=data_train).fit()

        rmse_train.append(rmse(data_train[y], model.predict(data_train)))
        rmse_test.append(rmse(data_test[y], model.predict(data_test)))

    if average_rmse:
        rmse_train = np.mean(rmse_train)
        rmse_test = np.mean(rmse_test)

    return {
        "RMSE": rmse_alldata,
        "R-squared": rsquared,
        "BIC": bic,
        "Coefficients": n_coefficients,
        "Training RMSE": rmse_train,
        "Test RMSE": rmse_test,
    }


import statsmodels


def point_predict_with_conf_int(
    regression: statsmodels.regression.linear_model.RegressionResultsWrapper,
    new_datapoint: pd.DataFrame,
    interval_precision=0.95,
    round_n=2,
) -> dict:
    """
    Does point prediction and interval prediction for a new datapoint.

        Parameters
    ----------
    regression : statsmodels.regression.linear_model.RegressionResultsWrapper
        Fitted regression model.
    new_datapoint : pd.DataFrame
        Database containing a new observation.
    interval_precision : float, default=0.95
        Precision of interval prediction.
    round_n: int, default=2
        Decimals to round floats in output.
    """

    summaryframe = regression.get_prediction(new_datapoint).summary_frame(
        alpha=1 - interval_precision
    )

    point_prediction = round(summaryframe["mean"].values[0], round_n)

    conf_int = [
        round(i, round_n)
        for i in summaryframe[["obs_ci_lower", "obs_ci_upper"]].values[0]
    ]

    if round_n == 0:
        point_prediction = int(point_prediction)
        conf_int = [int(i) for i in conf_int]
    else:
        pass

    return {
        "Point prediction": point_prediction,
        f"Prediction Interval ({round(interval_precision*100)}%)": conf_int,
    }


class Error(Exception):
    """Base class for other exceptions"""

    pass


class ConfintError(Error):
    """
    Error raised when a confidence interval
    does not match with required format.
    """

    def __init__(
        self,
        message="Confidence intervals are two numbers, so len(conf_int) must be 2.",
    ):
        self.message = message
        super().__init__(self.message)


def format_confidence_interval(conf_int: List[float], round_n=2) -> str:
    """Format confidence interval.

        Parameters
    ----------
    conf_int: np.array
        Array, consisting upper and upper confidence interval values.
    round_n: int, default=2
        Decimals to round floats in output.
    """
    if len(conf_int) != 2:
        raise ConfintError
    elif round_n == 0:
        return "[" + "–".join([str(int(round(i, round_n))) for i in conf_int]) + "]"
    else:
        return "[" + "–".join([str(round(i, round_n)) for i in conf_int]) + "]"


def create_sample_frame(
    vector: np.array, sample_size: int, n_samples=10000, with_replacement=False, seed=42
) -> List[np.array]:
    """
    Function for a specified number of samples.
    Draws a specified number of observations from a vector, either with or without replacement.
    Returns the matrix of samples.

    Used in `ch05-stock-market-loss-generalize.ipynb`

       Parameters
    ----------
    vector : np.array
        Vector of observations.
    sample_size : int
        Sample size, you want to draw.
        Set it len(vector) for bootstrap sampling.
    n_samples : int, default=10000
        Number of samples.
    with_replacement : bool, default=False
        Whether to perform sampling with or without
        replacement. Set `True` for bootstrap sampling.
    seed : int,default=42
        Random seed for reproducibility.
    """

    rng = np.random.default_rng(seed)
    sample_frame = np.zeros((n_samples, sample_size))
    for i in range(n_samples):
        sample_frame[i] = rng.choice(vector, size=sample_size, replace=with_replacement)

    return sample_frame


def add_margin(ax, x=0.05, y=0.05) -> None:
    """
    This will, by default, add 5% to the x and y margins to matplotlib plots.
    You can customise this using the x and y arguments when you call it.

       Parameters
    ----------
    ax : matplotlib.axes._subplots.AxesSubplot
        Vector of observations.
    x : float,default=0.05
        Margin to add for x axis.
    y : float,default=0.05
        Margin to add for x axis.
    """

    xlim = ax.get_xlim()
    ylim = ax.get_ylim()

    xmargin = (xlim[1] - xlim[0]) * x
    ymargin = (ylim[1] - ylim[0]) * y

    ax.set_xlim(xlim[0] - xmargin, xlim[1] + xmargin)
    ax.set_ylim(ylim[0] - ymargin, ylim[1] + ymargin)


def pool_and_categorize_continuous_variable(
    series: pd.Series, pools: List[tuple], categories: list, closed: str = "left"
):
    """
    Categorize a continuous variable based on defined intervals (pools) and corresponding categories.

    Parameters:
    series (pd.Series): The input series with continuous values to categorize.
    pools (list of tuples): A list of tuples representing the (min, max) values for each interval.
    categories (list): A list of categories corresponding to each interval in `pools`.
    closed (str): Indicates whether the intervals are closed on the 'left', 'right', 'both', or 'neither' sides.

    Returns:
    pd.Series: A series with categorized values based on the provided intervals and categories.

    Raises:
    ValueError: If the number of pools does not match the number of categories.
    """

    # Validate inputs
    if len(pools) != len(categories):
        raise ValueError("The number of pools and categories must be the same.")

    # Create bins using IntervalIndex from pools
    bins = pd.IntervalIndex.from_tuples(pools, closed=closed)

    # Perform the cut operation and map the intervals to categories
    categories_cut = pd.cut(series, bins=bins)
    categories_map = {
        interval: category for interval, category in zip(bins, categories)
    }

    return categories_cut.map(categories_map)


from io import StringIO
from IPython.display import Image, display
from sklearn.tree import export_graphviz, DecisionTreeRegressor
import pydotplus


def plot_decision_tree(model, **kwargs):
    dot_data = StringIO()
    export_graphviz(model, dot_data, **kwargs)
    graph = pydotplus.graph_from_dot_data(dot_data.getvalue())
    display(Image(graph.create_png()))


def plot_model_predictions(
    model, df, x_col: str, y_col: str, resolution=100, color=color[1]
):
    """
    Plots a scatterplot of data and overlays model predictions.

    - Uses horizontal stepwise predictions for DecisionTree-based models.
    - Uses a smooth line for continuous models (e.g., OLS, Logit).

    Parameters:
    - model: A fitted model with `.predict()`.
    - df: Pandas DataFrame containing the data.
    - x_col: Name of the predictor variable (e.g., "Age").
    - y_col: Name of the target variable (e.g., "Price").
    - color: Color for the prediction line/steps.
    - resolution: Number of points for smooth predictions.
    """
    # Scatter plot
    plt.figure(figsize=(8, 6))
    sns.scatterplot(data=df, x=x_col, y=y_col, alpha=0.6)

    # Generate sorted unique x values
    x_vals = np.linspace(df[x_col].min(), df[x_col].max(), resolution).reshape(-1, 1)
    x_vals_df = pd.DataFrame(x_vals, columns=[x_col])
    y_preds = model.predict(x_vals_df)

    # Decision Tree: Draw only horizontal lines
    if isinstance(model, DecisionTreeRegressor):
        for i in range(len(x_vals) - 1):
            plt.hlines(
                y=y_preds[i],
                xmin=x_vals[i],
                xmax=x_vals[i + 1],
                colors=color,
                linewidth=2,
            )
        # Extend last step to the max x value
        plt.hlines(
            y=y_preds[-1],
            xmin=x_vals[-1],
            xmax=df[x_col].max(),
            colors=color,
            linewidth=2,
        )

    else:
        plt.plot(x_vals, y_preds, color=color, linewidth=2)

    plt.xlim(0, 26)
    plt.xticks(range(0, 26, 5))
    plt.ylim(0, 20000)
    plt.yticks(range(0, 20001, 2500))
    plt.xlabel("Age (years)")
    plt.ylabel("Price (US dollars)")
    plt.show()


import sklearn
import seaborn as sns
import matplotlib.ticker as mticker
import math
import shap


def plot_variable_importance(
    data: pd.DataFrame,
    x: str = "imp_percentage",
    y: str = "varname",
    title: str | None = None,
):
    data = data.sort_values(by=x, ascending=False)

    sns.scatterplot(data=data, x=x, y=y, s=60)

    # Add horizontal lines from 0 to imp_percentage
    for _, row in data.iterrows():
        plt.hlines(y=row[y], xmin=0, xmax=row[x], linewidth=2.5)
    xtick_max = data[x].max()
    if xtick_max > 0.2:
        xtick_diff = 0.1
    else:
        xtick_diff = 0.05
    plt.xticks(np.arange(0, xtick_max, xtick_diff))
    plt.gca().xaxis.set_major_formatter(mticker.PercentFormatter(xmax=1, decimals=0))
    plt.title(title)
    plt.xlabel("Importance (Percent)")
    plt.ylabel("Variable Name")
    plt.show()


def plot_partial_dependence(
    model: sklearn.base.BaseEstimator,
    data: pd.DataFrame,
    variable: str,
    varlabel: str,
    kind: str = "average",  # "average", "individual", or "both"
):
    if kind not in {"average", "individual", "both"}:
        raise ValueError("kind must be one of {'average', 'individual', 'both'}")

    pd_results = sklearn.inspection.partial_dependence(
        model,
        data,
        [variable],
        kind=kind,
    )

    grid_values = pd_results["grid_values"][0]

    linestyle = "none" if grid_values.dtype == "object" else "-"

    if kind in {"individual", "both"}:
        ice = pd_results["individual"][0]

        for i in range(ice.shape[0]):
            if i % 100 == 0:
                ice_df = pd.DataFrame(
                {
                    "grid_values": grid_values,
                    "ice": ice[i, :],
                }
                )

                sns.pointplot(
                    data=ice_df,
                    x="grid_values",
                    y="ice",
                    alpha=0.25,
                    linestyle=linestyle,
                    scale=0.3 if linestyle == "-" else 0.5,
                    color=color[1]
                )

    if kind in {"average", "both"}:
        avg = pd_results["average"][0]

        pdp_df = pd.DataFrame(
            {
                "grid_values": grid_values,
                "average": avg,
            }
        )

        sns.pointplot(
            data=pdp_df,
            x="grid_values",
            y="average",
            scale=0.8,
            linestyle=linestyle,
        )

    if kind in {'both', 'individual'}:
        ymax = math.ceil(pd_results['individual'][0][list(range(0, pd_results['individual'][0].shape[0], 100))].max() / 10) * 10
        ymin = math.floor(pd_results['individual'][0][list(range(0, pd_results['individual'][0].shape[0], 100))].min() / 10) * 10
    else:
        ymax = math.ceil(avg.max() / 10) * 10
        ymin = math.floor(avg.min() / 10) * 10

    plt.ylim(ymin, ymax)
    plt.yticks(np.arange(ymin, ymax + 1, 10 if kind == 'average' else 30))

    plt.grid(axis="x", linestyle="-", alpha=0.7)
    plt.xlabel(varlabel)
    plt.ylabel("Predicted price")
    plt.show()


def plot_shap_interactions(features: tuple[str, str], shap_values: shap.Explanation):
    fig, axes = plt.subplots(
        1, len(features), figsize=(5 * len(features), 4), sharey=True
    )
    fig.suptitle(
        f"SHAP values and best proposed interaction for {', '.join(label for _, label in features)}",
        fontsize=15,
    )

    for ax, (feature, label) in zip(axes, features):
        shap.plots.scatter(
            shap_values[:, feature],
            color=shap_values,
            show=False,
            cmap=plt.get_cmap("viridis_r"),
            ax=ax,
        )
        ax.set_xlabel(label)
        ax.set_xlim(-0.2, 1.2)
        ax.set_xticks([0, 1], labels=[0, 1])
        ax.set_ylabel(None)

    axes[0].set_ylabel("SHAP values")
    plt.show()
