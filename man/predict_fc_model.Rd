% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict_ml_model.R
\name{predict_fc_model}
\alias{predict_fc_model}
\title{Predicts the forecasting method for the inputed time series.}
\usage{
predict_fc_model(fitted_model, time_series, ts_taxonomy = "v1")
}
\arguments{
\item{fitted_model}{A machine learning classification training model}

\item{time_series}{Either a vector, time series or data.frame object
representing a time series is allowed.}

\item{ts_taxonomy}{Either 'v1' or 'v2'. v1 uses the default time series
taxonomy and v2 the feature selected ts taxonomy of the tstaxonomyr package.}
}
\value{
The predicted class for the inputed new time series
\code{time_series}.
}
\description{
This is a function to predict the forecasting method based on the the
inputed training method \code{fitted_model} for the new data \code{new_data}.
Only the following caret models are supported: 'nnet', 'xgbLinear', 'xgbTree',
'svmLinear', 'svmPoly', 'ann', 'catboost', 'C5.0'. As input is only required
an time series object and a fitted classification model of class 'train'.
}
\examples{
predict_fc_model()
}
