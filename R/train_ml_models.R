

#' Generates a classification svm trainings model
#'
#' This is a function to generate a svm classification model for the
#' 1000 time series from the tstaxonomyr R package. The best fitted svm model is
#' identified based on the k-fold \code{cv_nfold} cross-validation and the
#' number of rounds \code{n_round}. The linear method 'svmLinear' and non-linear
#' method 'svmPoly' are both fitted and afterwards the one with the lower MAPE
#' is taken and returned. As input is only required the \code{n_round},
#' \code{cv_nfold} and \code{tune_length} with a number between 1 and 100. Also,
#'  for \code{ts_taxonomy}  only 'v1' for the basic taxonomy or 'v2' for the
#'  ligther feature selected taxonomy of the tstaxonomyr R package are allowed.
#' Otherwise the function returns an error message.
#'
#' @param tune_length Number of model tuning intervals.
#' @param n_round Number of cross-validation rounds.
#' @param cv_nfold Number of folds of cross-validation.
#' @param ts_taxonomy Either 'v1' or 'v2'. v1 uses the default time series
#' taxonomy and v2 the feature selected ts taxonomy of the tstaxonomyr package.
#' @return The best fitted svm classification model.
#' @examples
#' fitted_model <- train_svm()
#' @export
train_svm <- function(n_round = 10, cv_nfold = 10,
                      tune_length = 10, ts_taxonomy = "v1") {

  # If else clause to check the input svm_method
  if (ts_taxonomy %in% c("v1","v2") & n_round %in% c(1:100)
      & cv_nfold %in% c(1:100) & tune_length %in% c(1:100)) {


    if (ts_taxonomy == "v1") {
      features <- colnames(tsfcmethodr::ts_taxonomy_results)[which(colnames
                              (tsfcmethodr::ts_taxonomy_results) != "ts_name")]
      ts_fc_data <- cbind(tsfcmethodr::ts_taxonomy_results[, features],
      data.frame(best_model = tsfcmethodr::ts_fc_evaluation_results[, "best_model"]))
    } else {
      features <- colnames(tsfcmethodr::ts_fs_taxonomy_results)[which(colnames
                          (tsfcmethodr::ts_fs_taxonomy_results) != "ts_name")]
      ts_fc_data <- cbind(tsfcmethodr::ts_fs_taxonomy_results[, features],
      data.frame(best_model = tsfcmethodr::ts_fc_evaluation_results[, "best_model"]))
    }
    class_label <- "best_model"

    # Preprocess the data for the classification process
    prep_ts_fc_data <- fc_data_preparation(ts_fc_data, class_label)
    features <- colnames(prep_ts_fc_data)

    # Combine preprocessed data with class_label
    ts_fc_data <- cbind(as.data.frame(prep_ts_fc_data),
                        data.frame(label = ts_fc_data[, class_label]))
    ts_fc_data[, "label"] <- as.factor(ts_fc_data[, "label"])

    # Fit the tuning parameters
    fit_control <- caret::trainControl(method = "repeatedcv",
                                       number = cv_nfold,
                                       repeats = n_round)

    max_accuracy <- 0
    for (method in c("svmLinear", "svmPoly")) {
      # Train and identify a fitting model
      svm_model <- caret::train(x = ts_fc_data[, features],
                                y = ts_fc_data$label,
                                       method = method,
                                       trControl = fit_control,
                                       preProcess = c('scale', 'center'),
                                       tuneLength = tune_length,
                                        linout = FALSE

      )
      # Define the model with the highest accuracy
      if(max(svm_model$results$Accuracy) > max_accuracy) {
        best_svm_model <- svm_model
        max_accuracy <- max(svm_model$results$Accuracy)
      }

    }

    return(best_svm_model)

  } else {
    stop("Only a data frame object with more than 10, a target
         variable with at least two classes and none NA values is allowed!")
  }

}

#' Generates a classification ann trainings model.
#'
#' This is a function to generate a ann classification model for the
#' 1000 time series from the tstaxonomyr R package. The best fitted ann model is
#' identified based on the k-fold \code{cv_nfold} cross-validation and the
#' number of rounds \code{n_round}. The method 'nnet' from 'caret' package is
#' fitted and afterwards returned. As input is only required the \code{n_round},
#' \code{cv_nfold} and \code{tune_length} with a number between 1 and 100. Also,
#' for \code{ts_taxonomy}  only 'v1' for the basic taxonomy or 'v2' for the
#' ligther feature selected taxonomy of the tstaxonomyr R package are allowed.
#' Otherwise the function returns an error message.
#'
#' @param tune_length Number of model tuning intervals.
#' @param n_round Number of cross-validation rounds.
#' @param cv_nfold Number of folds of cross-validation.
#' @param ts_taxonomy Either 'v1' or 'v2'. v1 uses the default time series
#' taxonomy and v2 the feature selected ts taxonomy of the tstaxonomyr package.
#' @return The best fitted ann classification model.
#' @examples
#' fitted_model <- train_ann()
#' @export
train_ann <- function(n_round = 10, cv_nfold = 10, tune_length = 10,
                      ts_taxonomy = "v1") {

  # If else clause to check the input svm_method
  if (ts_taxonomy %in% c("v1","v2") & n_round %in% c(1:100)
      & cv_nfold %in% c(1:100) & tune_length %in% c(1:100)) {

    if (ts_taxonomy == "v1") {
      features <- colnames(tsfcmethodr::ts_taxonomy_results)[which(colnames
        (tsfcmethodr::ts_taxonomy_results) != "ts_name")]
      ts_fc_data <- cbind(tsfcmethodr::ts_taxonomy_results[, features],
                          data.frame(best_model = tsfcmethodr::ts_fc_evaluation_results[, "best_model"]))
    } else {
      features <- colnames(tsfcmethodr::ts_fs_taxonomy_results)[which(colnames
        (tsfcmethodr::ts_fs_taxonomy_results) != "ts_name")]
      ts_fc_data <- cbind(tsfcmethodr::ts_fs_taxonomy_results[, features],
                          data.frame(best_model = tsfcmethodr::ts_fc_evaluation_results[, "best_model"]))
    }
    class_label <- "best_model"

    # Preprocess the data for the classification process
    prep_ts_fc_data <- fc_data_preparation(ts_fc_data, class_label)
    features <- colnames(prep_ts_fc_data)

    # Combine preprocessed data with class_label
    ts_fc_data <- cbind(as.data.frame(prep_ts_fc_data),
                        data.frame(label = ts_fc_data[, class_label]))
    ts_fc_data[, "label"] <- as.factor(ts_fc_data[, "label"])

    # Fit the tuning parameters
    fit_control <- caret::trainControl(method = "repeatedcv",
                                       number = cv_nfold,
                                       repeats = n_round)

    ann_model <- caret::train(x = ts_fc_data[, features],
                              y = ts_fc_data$label,
                              method = "nnet",
                              trControl = fit_control,
                              preProcess = c('scale', 'center'),
                              tuneLength = tune_length,
                              linout = FALSE
                              )

    return(ann_model)

  } else {
    stop("Only a data frame object with more than 10, a target
         variable with at least two classes and none NA values is allowed!")
  }

}

#' Generates a classification xgboost trainings model.
#'
#' This is a function to generate a xgboost classification model for the
#' 1000 time series from the tstaxonomyr R package. The best fitted xgboost model
#' is identified based on the k-fold \code{cv_nfold} cross-validation and the
#' number of rounds \code{n_round}. The linear method 'xgbTree' and non-linear
#' method 'xgbLinear' are both fitted and afterwards the one with the lower MAPE
#' is taken and returned. As input is only required the \code{n_round},
#' \code{cv_nfold} and \code{tune_length} with a number between 1 and 100. Also,
#' for \code{ts_taxonomy}  only 'v1' for the basic taxonomy or 'v2' for the
#' ligther feature selected taxonomy of the tstaxonomyr R package are allowed.
#' Otherwise the function returns an error message.
#'
#' @param tune_length Number of model tuning intervals.
#' @param n_round Number of cross-validation rounds.
#' @param cv_nfold Number of folds of cross-validation.
#' @param ts_taxonomy Either 'v1' or 'v2'. v1 uses the default time series
#' taxonomy and v2 the feature selected ts taxonomy of the tstaxonomyr package.
#' @return The best fitted xgb classification model.
#' @examples
#' fitted_model <- train_xgb()
#' @export
train_xgb <- function(n_round = 10, cv_nfold = 10,
                      tune_length = 10, ts_taxonomy = "v1") {

  # If else clause to check the input svm_method
  if (ts_taxonomy %in% c("v1","v2") & n_round %in% c(1:100)
      & cv_nfold %in% c(1:100) & tune_length %in% c(1:100)) {

    if (ts_taxonomy == "v1") {
      features <- colnames(tsfcmethodr::ts_taxonomy_results)[which(colnames
          (tsfcmethodr::ts_taxonomy_results) != "ts_name")]
      ts_fc_data <- cbind(tsfcmethodr::ts_taxonomy_results[, features],
                          data.frame(best_model = tsfcmethodr::ts_fc_evaluation_results[, "best_model"]))
    } else {
      features <- colnames(tsfcmethodr::ts_fs_taxonomy_results)[which(colnames
          (tsfcmethodr::ts_fs_taxonomy_results) != "ts_name")]
      ts_fc_data <- cbind(tsfcmethodr::ts_fs_taxonomy_results[, features],
                          data.frame(best_model = tsfcmethodr::ts_fc_evaluation_results[, "best_model"]))
    }
    class_label <- "best_model"

    # Preprocess the data for the classification process
    prep_ts_fc_data <- fc_data_preparation(ts_fc_data, class_label)
    features <- colnames(prep_ts_fc_data)

    # Combine preprocessed data with class_label
    ts_fc_data <- cbind(as.data.frame(prep_ts_fc_data),
                        data.frame(label = ts_fc_data[, class_label]))
    ts_fc_data[, "label"] <- as.factor(ts_fc_data[, "label"])

    # Fit the tuning parameters
    fit_control <- caret::trainControl(method = "repeatedcv",
                                       number = cv_nfold,
                                       repeats = n_round)

    max_accuracy <- 0
    for (method in c("xgbTree","xgbLinear")) {
      # Train and identify a fitting model
      xgb_model <- caret::train(x = ts_fc_data[, features],
                                y = ts_fc_data$label,
                                method = method,
                                trControl = fit_control,
                                preProcess = c('scale', 'center'),
                                tuneLength = tune_length,
                                linout = FALSE
      )
      # Define the model with the highest accuracy
      if(max(xgb_model$results$Accuracy) > max_accuracy) {
        best_xgb_model <- xgb_model
        max_accuracy <- max(xgb_model$results$Accuracy)
      }

    }

    return(best_xgb_model)

  } else {
    stop("Only a data frame object with more than 10, a target
         variable with at least two classes and none NA values is allowed!")
  }

  }

#' Generates a classification catboost trainings model.
#'
#' This is a function to generate a catboost classification model for the
#' 1000 time series from the tstaxonomyr R package. The best fitted ann model is
#' identified based on the k-fold \code{cv_nfold} cross-validation and the
#' number of rounds \code{n_round}. The method 'catboost' from 'caret' package
#' is fitted and afterwards returned. As input is only required the
#' \code{n_round}, \code{cv_nfold} and \code{tune_length} with a number between
#' 1 and 100. Also, for \code{ts_taxonomy}  only 'v1' for the basic taxonomy or
#' 'v2' for the ligther feature selected taxonomy of the tstaxonomyr R package
#' are allowed. Otherwise the function returns an error message.
#'
#' @param tune_length Number of model tuning intervals.
#' @param n_round Number of cross-validation rounds.
#' @param cv_nfold Number of folds of cross-validation.
#' @param ts_taxonomy Either 'v1' or 'v2'. v1 uses the default time series
#' taxonomy and v2 the feature selected ts taxonomy of the tstaxonomyr package.
#' @return The best fitted catboost classification model.
#' @examples
#' fitted_model <- train_catboost()
#' @export
train_catboost <- function(n_round = 10, cv_nfold = 10, tune_length = 10, ts_taxonomy = "v1") {

  # If else clause to check the input svm_method
  if (ts_taxonomy %in% c("v1","v2") & n_round %in% c(1:100)
      & cv_nfold %in% c(1:100) & tune_length %in% c(1:100)) {

    if (ts_taxonomy == "v1") {
      features <- colnames(tsfcmethodr::ts_taxonomy_results)[which(colnames
            (tsfcmethodr::ts_taxonomy_results) != "ts_name")]
      ts_fc_data <- cbind(tsfcmethodr::ts_taxonomy_results[, features],
                          data.frame(best_model = tsfcmethodr::ts_fc_evaluation_results[, "best_model"]))
    } else {
      features <- colnames(tsfcmethodr::ts_fs_taxonomy_results)[which(colnames
           (tsfcmethodr::ts_fs_taxonomy_results) != "ts_name")]
      ts_fc_data <- cbind(tsfcmethodr::ts_fs_taxonomy_results[, features],
                          data.frame(best_model = tsfcmethodr::ts_fc_evaluation_results[, "best_model"]))
    }
    class_label <- "best_model"

    # Preprocess the data for the classification process
    prep_ts_fc_data <- fc_data_preparation(ts_fc_data, class_label)
    features <- colnames(prep_ts_fc_data)

    # Combine preprocessed data with class_label
    ts_fc_data <- cbind(as.data.frame(prep_ts_fc_data),
                        data.frame(label = ts_fc_data[, class_label]))
    ts_fc_data[, "label"] <- as.factor(ts_fc_data[, "label"])

    # Fit the tuning parameters
    fit_control <- caret::trainControl(method = "repeatedcv",
                                       number = cv_nfold,
                                       repeats = n_round,
                                       classProbs = TRUE)

    # Train and identify a fitting model
    catboost_model <- caret::train(x = ts_fc_data[, features],
                                   y = ts_fc_data$label,
                              method = catboost.caret,
                              trControl = fit_control,
                              preProcess = c('scale', 'center'),
                              tuneLength = tune_length,
                              linout = FALSE
    )

    return(catboost_model)

  } else {
    stop("Only a data frame object with more than 10, a target
         variable with at least two classes and none NA values is allowed!")
  }

}

#' Generates a rule based trainings model.
#'
#' This is a function to generate a C5.0 rule model for the 1000 time series
#' from the tstaxonomyr R package. The best fitted model is identified based
#' number of boosting trials \code{boosting_length}.
#' The method 'C5.0' from the 'C50' R package is fitted and afterwards returned.
#' As input is only required the \code{boosting_length} with a number between 1
#' and 100. Also, for \code{ts_taxonomy}  only 'v1' for the basic taxonomy or
#' 'v2' for the ligther feature selected taxonomy of the tstaxonomyr R package
#' are allowed. Otherwise the function returns an error message.
#'
#' @param boosting_length Number of model boosting intervals.
#' @param ts_taxonomy Either 'v1' or 'v2'. v1 uses the default time series
#' taxonomy and v2 the feature selected ts taxonomy of the tstaxonomyr package.
#' @return The trained rule model from the C5.0 algorithm.
#' @examples
#' fitted_model <- train_c50_rule_model()
#' @export
train_c50_rule_model <- function(boosting_length = 10, ts_taxonomy = "v1"){

  # If else clause to check the input svm_method
  if (ts_taxonomy %in% c("v1","v2") & boosting_length %in% c(1:100)) {

    if (ts_taxonomy == "v1") {
      features <- colnames(tsfcmethodr::ts_taxonomy_results)[which(colnames
              (tsfcmethodr::ts_taxonomy_results) != "ts_name")]
      ts_fc_data <- cbind(tsfcmethodr::ts_taxonomy_results[, features],
      data.frame(best_model = tsfcmethodr::ts_fc_evaluation_results[, "best_model"]))
    } else {
      features <- colnames(tsfcmethodr::ts_fs_taxonomy_results)[which(colnames
             (tsfcmethodr::ts_fs_taxonomy_results) != "ts_name")]
      ts_fc_data <- cbind(tsfcmethodr::ts_fs_taxonomy_results[, features],
      data.frame(best_model = tsfcmethodr::ts_fc_evaluation_results[, "best_model"]))
    }

    rule_model <- C5.0(best_model ~ ., data = ts_fc_data,
                       rules = TRUE, trials = boosting_length)

    return(rule_model)

  } else {
    stop("Only a data frame object with more than 10, a target
         variable with at least two classes and none NA values is allowed!")
  }

}
