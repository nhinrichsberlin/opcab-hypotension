import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

from sklearn.base import clone
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import SplineTransformer, StandardScaler
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import RandomizedSearchCV

from python.utils import custom_logger

from python import (
    TREATMENT,
    TREATMENT_TRANSFORMED,
    TREATMENT_INDEX,
    OUTCOME,
    TREATMENT_INDEX_TRANSFORMED,
    CONFOUNDERS,
    CONFOUNDER_LABELS
)

LOGGER = custom_logger('gps.py')

DATA = pd.read_csv('data/opcab_analysis_with_gps.csv', sep=';')


def gps_search():

    pipe = Pipeline([
        ('splines', SplineTransformer(knots='quantile')),
        ('scale', StandardScaler()),
        ('logreg', LogisticRegression(penalty='l2', max_iter=10000))
    ])

    hyperparams = {
        'logreg__C': np.logspace(-2, 1, 100)
    }

    search = RandomizedSearchCV(
        estimator=pipe,
        param_distributions=hyperparams,
        scoring='roc_auc',
        cv=5,
        n_iter=100,
        n_jobs=-1
    )

    return search


def plot_dose_response_function(drf: pd.Series,
                                drf_unadjusted: pd.Series):

    _ = plt.figure(figsize=(8, 8), dpi=500)

    _ = plt.scatter(DATA[TREATMENT],
                    DATA[OUTCOME],
                    s=0.1,
                    color='black',
                    label='Observed outcomes')

    _ = plt.plot(TREATMENT_INDEX,
                 drf,
                 linewidth=5,
                 color='darkred',
                 label='GPS-weighted DRF')

    _ = plt.plot(TREATMENT_INDEX,
                 drf_unadjusted,
                 linewidth=2,
                 alpha=0.25,
                 linestyle='dashed',
                 color='blue',
                 label='Unweighted DRF')

    _ = plt.xticks(np.arange(0, TREATMENT_INDEX.max() + 5, 10), fontsize=10, rotation=45)
    _ = plt.yticks(np.arange(0, 1.1, 0.1))
    _ = plt.grid(alpha=0.2, which='both')    # noqa
    _ = plt.title('Dose response function (DRF)', fontsize=12)

    _ = plt.xlabel('Total minutes below BPM 60 mmHg')

    _ = plt.ylabel('Risk of composite outcome')

    _ = plt.legend(loc='center', fontsize=10)

    _ = plt.savefig(f'figures/gps/gps_weighted_drf.png', bbox_inches='tight')    # noqa


def weighted_pearson_correlation(x, y, w):

    if w is None:
        w = np.ones_like(x)

    x_mean = np.average(x, weights=w)
    y_mean = np.average(y, weights=w)

    x_diff = x - x_mean
    y_diff = y - y_mean

    x_var = np.sum(w * (x_diff ** 2))
    y_var = np.sum(w * (y_diff ** 2))

    return np.sum(w * x_diff * y_diff) / np.sqrt(x_var * y_var)


def plot_correlation_diagnostics(corr_weighted: pd.Series,
                                 corr_unweighted: pd.Series):

    _ = plt.figure(figsize=(8, 8), dpi=500)

    # fix the order
    corr_unweighted = corr_unweighted.abs().sort_values()
    corr_weighted = corr_weighted.reindex(corr_unweighted.index)

    _ = plt.xlim(-0.01, 0.26)

    _ = plt.scatter(corr_weighted.abs(),
                    corr_weighted.index,
                    s=12,
                    marker='^',
                    color='darkred',
                    label=f'GPS-weighted correlation')

    _ = plt.scatter(corr_unweighted.abs(),
                    corr_unweighted.index,
                    s=12,
                    marker='o',
                    color='blue',
                    label=f'Unweighted correlation')

    _ = plt.axvline(0.1,
                    color='black',
                    alpha=0.25)

    _ = plt.xticks(np.linspace(0, 0.3, 7))
    _ = plt.grid(alpha=0.1)    # noqa

    _ = plt.xlabel('Absolute correlation coefficient', fontsize=10)

    _ = plt.legend(loc=(0.4, 0.5), fontsize=10)

    _ = plt.yticks(ticks=corr_weighted.index,
                   labels=[CONFOUNDER_LABELS[i] for i in corr_weighted.index],
                   fontsize=10)

    _ = plt.savefig(f'figures/gps/pearson_correlation.png', bbox_inches='tight')    # noqa


def main():

    LOGGER.info(f'Computing the dose response function.')

    # check how well weights balance confounders
    correlation_weighted = pd.Series([
        weighted_pearson_correlation(x=DATA[TREATMENT],
                                     y=DATA[c],
                                     w=DATA[f'gps_weights'])
        for c in CONFOUNDERS
    ], index=CONFOUNDERS)

    LOGGER.info(f'Weighted correlation: '
                f'mean={correlation_weighted.abs().mean()}, '
                f'max={correlation_weighted.abs().max()}')

    correlation_unweighted = pd.Series([
        weighted_pearson_correlation(x=DATA[TREATMENT],
                                     y=DATA[c],
                                     w=None)
        for c in CONFOUNDERS
    ], index=CONFOUNDERS)

    # Run a (weighted) regression of the treatment on the outcome
    df_gps = DATA[[TREATMENT_TRANSFORMED]].copy()

    # we start without weights to optimize hyperparameters
    search_unweighted = gps_search()
    search_unweighted.fit(df_gps, DATA[OUTCOME])

    search_weighted = clone(search_unweighted)
    search_weighted.fit(df_gps, DATA[OUTCOME], logreg__sample_weight=DATA[f'gps_weights'])

    # Estimate the dose-response-function at the desired indices
    x_treatment = pd.DataFrame(TREATMENT_INDEX_TRANSFORMED, columns=[TREATMENT_TRANSFORMED])

    drf = search_weighted.predict_proba(x_treatment)[:, -1]
    drf_unweighted = search_unweighted.predict_proba(x_treatment)[:, -1]

    LOGGER.info('Dose response function successfully estimated.')

    # plot the dose response function
    plot_dose_response_function(drf, drf_unweighted)

    # plot diagnostics on how well the GPS weighting reduced correlation between treatment and confounders
    plot_correlation_diagnostics(correlation_weighted, correlation_unweighted)


if __name__ == '__main__':
    main()
