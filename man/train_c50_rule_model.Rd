% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/train_ml_models.R
\name{train_c50_rule_model}
\alias{train_c50_rule_model}
\title{Generates a rule based trainings model.}
\usage{
train_c50_rule_model(boosting_length = 10, ts_taxonomy = "v1")
}
\arguments{
\item{boosting_length}{Number of model boosting intervals.}

\item{ts_taxonomy}{Either 'v1' or 'v2'. v1 uses the default time series
taxonomy and v2 the feature selected ts taxonomy of the tstaxonomyr package.}
}
\value{
The trained rule model from the C5.0 algorithm.
}
\description{
This is a function to generate a C5.0 rule model for the 1000 time series
from the tstaxonomyr R package. The best fitted model is identified based
number of boosting trials \code{boosting_length}.
The method 'C5.0' from the 'C50' R package is fitted and afterwards returned.
As input is only required the \code{boosting_length} with a number between 1
and 100. Also, for \code{ts_taxonomy}  only 'v1' for the basic taxonomy or
'v2' for the ligther feature selected taxonomy of the tstaxonomyr R package
are allowed. Otherwise the function returns an error message.
}
\examples{
fitted_model <- train_c50_rule_model()
}
