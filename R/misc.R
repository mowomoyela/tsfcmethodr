#' Prepares a data frame object for classification ml method
#'
#' This is a function to prepares a data frame object for the classification
#' ml process. All date cols are deleted. Also, all character and factor cols
#' are transformed by binarization through one hot encoding.
#' As input is only required an object from the class data.frame
#' Otherwise the function returns an error message.
#'
#' @param ts_fc_data A time series or data frame object.
#' @param class_label A character value containing a column name of the
#' inputed data frame \code{ts_fc_data}.
#' @return A data frame object containg the prepared data.
#' If the input is not a time series or data frame object, an error message
#' is returned.
#' @examples
#' library(mlbench)
#' fc_data_preparation(train_ann = mlbench::PimaIndiansDiabetes,
#' class_label = 'diabetes')
fc_data_preparation <- function(ts_fc_data, class_label) {

  if(!is.data.frame(ts_fc_data)) {
    stop("A data frame object is required as input!")
  }

  # Preprocess the data without the class_label
  prep_ts_fc_data <- ts_fc_data[,colnames(ts_fc_data)[which(colnames(ts_fc_data)
                                                            != class_label)]]

  # All character values have to be transformed as factor
  for (colname in names(Filter(is.character, prep_ts_fc_data))) {
    prep_ts_fc_data[,colname] <- as.factor(prep_ts_fc_data[, colname])
  }

  # All date cols have to be deleted
  for (colname in c(names(Filter(is.Date, prep_ts_fc_data)),
                    names(Filter(is.POSIXt, prep_ts_fc_data)))) {
    prep_ts_fc_data[,colname] <- NULL
  }

  # One-Hot-Encode unordered factor columns of a data.table
  encoded_data <- mltools::one_hot(data.table::as.data.table(prep_ts_fc_data)
                                  , cols = "auto", dropCols = TRUE,
                                  dropUnusedLevels = TRUE)
  encoded_data <-as.data.frame(encoded_data)


  # Removes predictors that have zero variance predictors
  if(nrow(encoded_data) > 2) {
    encoded_data <- as.data.frame(encoded_data[,-c(caret::nearZeroVar(encoded_data)
                                                   , ncol(encoded_data))])
  }

  return(encoded_data)
}


#' Classifies a ts based on the tstaxonomyr R package.
#'
#' This is a function to classify a time series data by the tstaxonomyr R
#' package. It requires a time series, vector or a data.frame object which
#' represents a multivariate time series. The time series object is classified
#' on several (statistical) features of the ts taxonomy from the
#' 'tstaxonomyr' R package.
#'
#' @param ts Either a vector, time series or data.frame object representing
#' a time series is allowed.
#' @param ts_taxonomy Either 'v1' or 'v2'. v1 uses the default time series
#' taxonomy and v2 the feature selected ts taxonomy of the tstaxonomyr package.
#' @return A data frame, containing all scaled feature factor values.
#' If the input \code{ts} is not a vector, ts or data.frame object
#' an error message is returned.
#' @examples
#' classify_new_ts_taxonomy(ts = datasets::BJsales)
classify_new_ts_taxonomy <-function(ts, ts_taxonomy = "v1"){

  # If-else clause to check inputed objects
  if (is.ts(ts) | is.vector(ts) |
    is.data.frame(ts)) {
    if (ts_taxonomy == "v1") {
      classified_ts <- tstaxonomyr::classify_ts(ts = ts,
                                                na_option = "mean",
                                                taxonomy_type = ts_taxonomy)
      train_data <- tsfcmethodr::ts_taxonomy_results
    } else if (ts_taxonomy == "v2") {
      classified_ts <- tstaxonomyr::classify_ts(ts = ts,
                                                   na_option = "mean",
                                                   taxonomy_type = ts_taxonomy)
      train_data <- tsfcmethodr::ts_fs_taxonomy_results
    } else {
      stop("Unallowed ts_taxonomy input paramter!")
    }
  } else {
    stop("A data frame, time series or vector object is expected!")
  }

  # Create new data frame
  classified_ts_df <- as.data.frame(matrix(NA, nrow = 1, ncol =
                                             length(classified_ts)))
  colnames(classified_ts_df) <- names(classified_ts)

  # Fill the data frame with the time series results
  for(elem in names(classified_ts)){
    classified_ts_df[, elem] <- as.character(paste(classified_ts[[elem]],
                                                   collapse = ' '))
  }

  # Generate the factor values
  classified_ts_df <- rbind(train_data[, which(colnames(train_data)
                                              != "ts_name")], classified_ts_df)
  # All character values have to be transformed as factor
  for (colname in names(Filter(is.character, classified_ts_df))) {
    classified_ts_df[,colname] <- as.factor(classified_ts_df[, colname])
  }
  # Get the classified and transformed data
  classified_ts_df <- classified_ts_df[nrow(classified_ts_df), ]

  return(classified_ts_df)
}
