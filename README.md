
tsfcmethodr
===========

Provides four different classification machine learning methods and one C5.0 rule model to predict the best performing forecasting method for time series. The four different classification methods are: xgboost, cat boost, svm and ann. This package contains labeled time series data to train the models for the prediction of new time series data.

Usage
-----

``` r
# Initialize the R package
library(tsfcmethodr)
# Example of a xgboost classification model with the basic ts 
# taxonomy from tstaxonomyr R package --------
  # Train a xgboost model 
  fitted_model <- train_xgb(n_round = 10, cv_nfold = 10, 
                            tune_length = 10, ts_taxonomy = "v1")
  # Predict best performing forecasting method for a new classified ts
  ts_sales = datasets::BJsales
  prediction <- predict_fc_model(fitted_model, ts_sales, "v1")
  prediction
  
# Example of a svm classification model with the ligther feature 
# selected ts taxonomy from tstaxonomyr R package --------
  # Train a svm model 
  fitted_model <- train_svm(n_round = 10, cv_nfold = 10,
                            tune_length = 10, ts_taxonomy = "v2")
  # Predict best performing forecasting method for a new classified ts
  ts_sales = datasets::BJsales
  prediction <- predict_fc_model(fitted_model, ts_sales, "v2")
  prediction
```

Installation
------------

You can install the **development** version 1.0.0 from [Github](https://github.com/mowomoyela/tsfcmethodr) with:

``` r
devtools::install_github("mowomoyela/tsfcmethdr")
```

Overview
--------

All provided functions of this package:

-   Version 1.0.0
    -   train\_svm: Generates a classification svm trainings model.
    -   train\_ann: Generates a classification ann trainings model.
    -   train\_xgb: Generates a classification xgboost trainings model.
    -   train\_c50\_rule\_model: Generates a C5.0 rule model.
    -   train\_catboost: Generates a classification catboost trainings model.
    -   predict\_fc\_model: Predicts the best performing forecasting method for a new time series.

License
-------

This package is free and open source software, licensed under GPL-2.
