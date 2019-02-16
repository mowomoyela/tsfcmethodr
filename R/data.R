#' A data frame of 1000 different classified time series data.
#'
#' A data frame containing 1000 different classified univariate and
#' multivariate time series data from the tstaxonomyr R package.
#' Each row represents one single time series that is classified on 24 different
#' (statistical) features which are listed in the following item description.
#' All features/columns are factor values of the different classification
#' classes.
#'
#' @format A data frame with 1000 elements.
#' Each row represents a time series object that is classified on the following
#' (statistical features) from the tstaxonomyr R package representing the
#' columns of the data frame:
#' \describe{
#'   \item{Skewness}{The skewness factor of the time series}
#'   \item{Kurtosis}{The kurtosis factor of the time series}
#'   \item{Trend}{The trend factor of the time series}
#'   \item{Autocorrelation}{The autocorrelation factor of the time series}
#'   \item{Mean}{The mean factor of the time series}
#'   \item{StandardDeviation}{The sd factor of the time series}
#'   \item{NumberOfObservations}{The length of the time series}
#'   \item{NonLinearity}{The non-linearity factor of the time series}
#'   \item{Seasonality}{The seasonality factor of the time series}
#'   \item{Periodicity}{The periodicity factor of the time series}
#'   \item{Chaos}{The chaos factor of the time series}
#'   \item{Entropy}{The entropy factor of the time series}
#'   \item{SelfSimilarity}{The self-similarity factor of the time series}
#'   \item{feature_dtwdistance}{The dynamic time warping distance to other
#'   time series blocks}
#'   \item{TurningPoints}{The number of turning points of the time series}
#'   \item{PartialAutocorrelation}{The partial autocorrelation factor of
#'   the time series}
#'   \item{Variance}{The variance factor of the time series}
#'   \item{Outliers}{The number of outliers of the time series}
#'   \item{StepChanges}{The step changes factor of the time series}
#'   \item{Peaks}{The number of peaks of the time series}
#'   \item{DurbinWatsonTest}{The dw test factor of the time series}
#'   \item{calculate_determination_coefficient}{The R^2 factor of the time
#'   series}
#'   \item{NumberOfAttributes}{The number of features of the time series}
#'   \item{ts_name}{The name of the time series from the tstaxonomyr package}
#' }
#' @source
#' tstaxonomyr R package \url{https://github.com/mowomoyela/tstaxonomyr}
"ts_taxonomy_results"

#' A data frame of 1000 different classified time series data.
#'
#' A data frame containing 1000 different classified univariate and
#' multivariate time series data from the tstaxonomyr R package.
#' Each row represents one single time series that is classified on XX different
#' (statistical) features which are listed in the following item description.
#' This time the time series data was classified on
#' the feature selection reduced taxonomy. All features/columns are factor
#' values of the different classification classes.
#'
#' @format A data frame with 1000 elements.
#' Each row represents a time series object that is classified on the following
#' 15 (statistical features) from the tstaxonomyr R package representing the
#' columns of the data frame. This time the time series data was classified on
#' the feature selection reduced taxonomy:
#' \describe{
#'   \item{Trend}{The trend factor of the time series}
#'   \item{Autocorrelation}{The autocorrelation factor of the time series}
#'   \item{Seasonality}{The seasonality factor of the time series}
#'   \item{Periodicity}{The periodicity factor of the time series}
#'   \item{Chaos}{The chaos factor of the time series}
#'   \item{Entropy}{The entropy factor of the time series}
#'   \item{TurningPoints}{The number of turning points of the time series}
#'   \item{PartialAutocorrelation}{The partial autocorrelation factor of
#'   the time series}
#'   \item{Outliers}{The number of outliers of the time series}
#'   \item{StepChanges}{The step changes factor of the time series}
#'   \item{Peaks}{The number of peaks of the time series}
#'   \item{DurbinWatsonTest}{The dw test factor of the time series}
#'   \item{calculate_determination_coefficient}{The R^2 factor of the time
#'   series}
#'   \item{NumberOfAttributes}{The number of features of the time series}
#'   \item{ts_name}{The name of the time series from the tstaxonomyr package}
#' }
#' @source
#' tstaxonomyr R package \url{https://github.com/mowomoyela/tstaxonomyr}
"ts_fs_taxonomy_results"

#' A data frame of 1000 time series data forecasting method evaluation.
#'
#' A data frame containing 1000 different univariate and
#' multivariate time series data from the tstaxonomyr R package. For each one
#' is given the best performing forecasting method out of the seven
#' possible ones: 'random walk', 'exponentiel smoothing', 'arima', 'svm',
#' 'ann', 'xgboost' and 'cart'.
#'
#' @format A data frame with 1000 elements.
#' Each row represents a time series object with it's best performing
#' forecasting method. The following columns are provided:
#' \describe{
#'   \item{ts_name}{The name of the time series from the tstaxonomyr package}
#'   \item{best_fc_method}{The best performing forecasting method}
#' }
"ts_fc_evaluation_results"


#' A fitted classification xgboost model from the caret R package
#'
#' A fitted classification xgboost model from the caret R packe based on the
#' two .R files: "ts_taxonomy_results" and "ts_fc_evaluation_results"
#' from this package. The "ts_taxonomy_results" are applied as training features
#' and the "ts_fc_evaluation_results" as prediction label. Thus the model is
#' able to predict for new "ts_taxonomy_results" data the best performing
#' forecasting method out of the seven possible from the
#' "ts_fc_evaluation_results" file.
#'
#' @format fitted xgboost model from the caret R package The method is
#' called 'xgbTree'. It is trained by cross-validation.
"fitted_ts_taxonomy_xgb_model"


#' A fitted classification xgboost model from the caret R package
#'
#' A fitted classification xgboost model from the caret R package based on the
#' two .R files: "ts_fs_taxonomy_results" and "ts_fc_evaluation_results"
#' from this package. The "ts_fs_taxonomy_results" are applied as training
#' features and the "ts_fc_evaluation_results" as prediction label. Thus the
#' model is able to predict for new "ts_fs_taxonomy_results" data the best
#' performing forecasting method out of the seven possible from the
#' "ts_fc_evaluation_results" file.
#'
#' @format fitted xgboost model from the caret R packet. The method is
#' called 'xgbTree'. It is trained by cross-validation.
"fitted_ts_fs_taxonomy_xgb_model"

#' A fitted classification c5.0 rule model from the c50 R package
#'
#' A fitted classification xgboost model from the c50 R package based on the
#' two .R files: "ts_fs_taxonomy_results" and "ts_fc_evaluation_results"
#' from this package. The "ts_fs_taxonomy_results" are applied as training
#' features and the "ts_fc_evaluation_results" as prediction label. Thus the
#' model is able to predict for new "ts_fs_taxonomy_results" data the best
#' performing forecasting method out of the seven possible from the
#' "ts_fc_evaluation_results" file.
#'
#' @format fitted c50 model from the c50 R package The method is
#' called 'c50'.
"fitted_ts_fs_taxonomy_c50_model"

#' A fitted classification c5.0 rule model from the c50 R package
#'
#' A fitted classification xgboost model from the caret R packe based on the
#' two .R files: "ts_taxonomy_results" and "ts_fc_evaluation_results"
#' from this package. The "ts_taxonomy_results" are applied as training features
#' and the "ts_fc_evaluation_results" as prediction label. Thus the model is
#' able to predict for new "ts_taxonomy_results" data the best performing
#' forecasting method out of the seven possible from the
#' "ts_fc_evaluation_results" file.
#'
#' @format fitted c50 model from the c50 R package The method is
#' called 'c50'.
"fitted_ts_taxonomy_c50_model"
