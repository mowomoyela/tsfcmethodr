
#' Predicts the forecasting method for the inputed time series.
#'
#' This is a function to predict the forecasting method based on the the
#' inputed training method \code{fitted_model} for the new data \code{new_data}.
#' Only the following caret models are supported: 'nnet', 'xgbLinear', 'xgbTree',
#' 'svmLinear', 'svmPoly', 'ann', 'catboost', 'C5.0'. As input is only required
#' an time series object and a fitted classification model of class 'train'.
#'
#' @param fitted_model A machine learning classification training model
#' @param time_series Either a vector, time series or data.frame object
#' representing a time series is allowed.
#' @param ts_taxonomy Either 'v1' or 'v2'. v1 uses the default time series
#' taxonomy and v2 the feature selected ts taxonomy of the tstaxonomyr package.
#' @return The predicted class for the inputed new time series
#' \code{time_series}.
#' @examples
#' predict_fc_model()
#' @export
predict_fc_model <- function(fitted_model, time_series, ts_taxonomy = "v1"){

  # If-else clause to check inputed objects
  if (((is.ts(time_series)) | is.vector(time_series) |
       is.data.frame(time_series)) & (ts_taxonomy == "v1" |
                                      ts_taxonomy == "v2")) {

    # Classify the new time_series
    new_data <- classify_new_ts_taxonomy(ts = time_series,
                                         ts_taxonomy = ts_taxonomy)
  } else {
    stop("A data frame, time series or vector object is expected!")
  }

  # If-else clause to check inputed objects
  # Check if the fitted_model is from class 'train'
  if (class(fitted_model)[1] != "train" & class(fitted_model)[1] != "C5.0") {
    stop("A machine learning model of class 'train' or 'C5.0' is required!")
    # Check if a caret method is inputed that is allowed
  } else if (!is.null(fitted_model$method)) {
    if(!(fitted_model$method %in% c("nnet", "xgbLinear", "xgbTree",
                                    "svmLinear", "svmPoly", "custom"))) {
      stop("Only the methods 'nnet', 'xgbLinear', 'xgbTree',
         'svmLinear', 'svmPoly', 'catboost' are allowed")
    }
    # Check if new data is a data frame
  } else if (!is.data.frame(new_data) |
             (nrow(new_data) - nrow(na.omit(new_data))) != 0) {
    stop("new_data has to be of class 'data frame' and none NA values are
         allowed")
  }

  if(class(fitted_model)[1] == "C5.0"){
    # Predict the final values
    classification_results <- predict(fitted_model, newdata = new_data)
  } else {
    # Preprocess the data for the classification process
    ts_fc_data <- fc_data_preparation(new_data, class_label = "")

    # Transform the prediction data into the trainings data format
    prediction_data <- matrix(NA,
                              nrow = nrow(ts_fc_data),
                              ncol = ncol(fitted_model$trainingData))
    prediction_data <- as.data.frame(prediction_data)
    colnames(prediction_data) <- colnames(fitted_model$trainingData)
    prediction_data[,".outcome"] <- NULL

    for(col in colnames(prediction_data)){
      if(col %in% colnames(ts_fc_data)){
        prediction_data[, col] <- ts_fc_data[, col]
      } else {
        prediction_data[, col] <- 0
      }
    }

    # Predict the final values
    classification_results <- predict(fitted_model, newdata = prediction_data)

  }


  # Assign the level names from the fitted model to the results
  for(elem in sort(unique(as.numeric(classification_results)))) {
    classification_results[which(classification_results == elem)] <-
      as.vector(fitted_model$levels)[elem]
  }

  return(classification_results)
}


