# Disentangling drivers & constructors in the F1 hybrid era

Repository containing code & presentation accompanying the manuscript _Bayesian Analysis of Formula One Race Results_. The scripts contain the following:

| Script              | Contents                                                         |
| :------------------ | :--------------------------------------------------------------- |
| `01_prep_data.R`    | Data preparation, data joining from database `f1db_csv`          |
| `02_process_data.R` | Data processing, status filtering, outcome computation, some EDA |
| `03_model.R`        | Creating and estimating models                                   |
| `04_compare.R`      | Performing model comparison                                      |
| `05_check.R`        | MCMC validation, posterior predictive checks                     |
| `06_infer.R`        | Inferences using posteriors of parameters                        |
| `07_predict.R`      | Counterfactual predictions                                       |

Picture sources are in the presentation notes.

Disclaimer: these ratings are the result of a statistical model and its accompanying simplifying assumptions, estimated using only position data from 2014-2020. Please do not take the ratings as absolute truth.
