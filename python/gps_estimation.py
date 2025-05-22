import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

from sklearn.linear_model import Ridge
from sklearn.model_selection import RandomizedSearchCV
from sklearn.preprocessing import SplineTransformer, StandardScaler
from sklearn.pipeline import Pipeline
from sklearn.compose import ColumnTransformer

from python import (
    TREATMENT,
    TREATMENT_TRANSFORMED,
    DATA,
    CONFOUNDERS,
    CONFOUNDERS_NUMERICAL
)
from python.utils import custom_logger

LOGGER = custom_logger('python/gps.py')


def build_pipeline():

    # define the pipeline
    # add splines to numerical features
    column_transformer = ColumnTransformer(
        transformers=[('splines',
                       SplineTransformer(knots='quantile'),
                       CONFOUNDERS_NUMERICAL)],
        remainder='passthrough'
    )

    # estimator
    pipe = Pipeline([('nonlinearities', column_transformer),
                     ('scale', StandardScaler()),
                     ('regress', Ridge(max_iter=10000, random_state=789))])

    return pipe


def plot_treatment_vs_predictions(model: RandomizedSearchCV):

    _ = plt.figure(figsize=(8, 8), dpi=500)

    true_values = DATA[TREATMENT]

    predictions = np.exp(model.predict(DATA[CONFOUNDERS])) - 1
    mse = np.mean((true_values - predictions.ravel()) ** 2)

    _ = plt.scatter(true_values,
                    predictions,
                    color='darkred',
                    s=3,
                    label=f'MSE={np.round(mse, 3)}')

    _ = plt.plot([DATA[TREATMENT].min(), DATA[TREATMENT].max()],
                 [DATA[TREATMENT].min(), DATA[TREATMENT].max()],
                 color='black',
                 linewidth=1)

    _ = plt.xlim(DATA[TREATMENT].min() - 1, DATA[TREATMENT].max() + 1)
    _ = plt.ylim(DATA[TREATMENT].min() - 1, DATA[TREATMENT].max() + 1)

    _ = plt.legend(loc=(0.05, 0.875))

    _ = plt.xlabel('Total time below BPM 60 mmHg')

    _ = plt.ylabel('Predicted values')

    _ = plt.xlim(DATA[TREATMENT].min(), DATA[TREATMENT].max())
    _ = plt.ylim(DATA[TREATMENT].min(), DATA[TREATMENT].max())

    _ = plt.title('Ridge regression')

    _ = plt.savefig(f'figures/gps/gps_regression_scatter.png')     # noqa


def main():

    # 1) Estimate the generalized propensity score
    # 1a) Build a regression model: y=treatment, x=confounders
    LOGGER.info('Running regression: treatment ~ confounders.')
    LOGGER.info(f'Number of confounders: {len(CONFOUNDERS)}')

    pipe = build_pipeline()

    # define random search
    search = RandomizedSearchCV(
        estimator=pipe,
        param_distributions={'regress__alpha': np.logspace(-4, 0, 1000)},
        n_iter=100,
        cv=5,
        n_jobs=-1,
        random_state=0,
        scoring='neg_mean_squared_error'
    )

    # run the search for the best hyperparameters
    search.fit(DATA[CONFOUNDERS], DATA[TREATMENT_TRANSFORMED])

    LOGGER.info(f'Hyperparameter tuning done. '
                f'MSE = {np.round(np.abs(search.best_score_), 4)}')

    # 1b) Use predictions to estimate the GPS
    LOGGER.info(f'Computing the GPS')

    DATA['fitted'] = search.predict(DATA[CONFOUNDERS])

    residuals_std = (DATA[TREATMENT_TRANSFORMED] - DATA[f'fitted']).std()

    DATA['gps'] = DATA.apply(
        lambda row: norm(loc=row[f'fitted'], scale=residuals_std).pdf(row[TREATMENT_TRANSFORMED]),
        axis=1
    )

    # add stabilizing weights
    DATA['weight_stabilizer'] = DATA.apply(
        lambda row: norm(
            loc=DATA[TREATMENT_TRANSFORMED].mean(),
            scale=DATA[TREATMENT_TRANSFORMED].std()
        ).pdf(row[TREATMENT_TRANSFORMED]),
        axis=1
    )

    # define gps-weights as stabilizers/gps
    DATA[f'gps_weights'] = DATA['weight_stabilizer'] / DATA['gps']

    LOGGER.info('GPS estimation done.')

    # plot quality of the prediction of treatment by confounders
    plot_treatment_vs_predictions(search)

    # store DATA because it contains the GPS
    DATA.to_csv('data/opcab_analysis_with_gps.csv', sep=';')


if __name__ == '__main__':
    main()
