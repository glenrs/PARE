#' A calculate Accuracy Function
#' 
#' This function allows you to calculate the Accuracy
#' @param labels ??? a factor object predictors threshold
#' @keywords labels, predictors, accuracy
#' @export
#' @examples
#' calculateAccuracy(2,3,5)


calculateAccuracy <- function(labels, predictors, threshold)
{
  sum(as.integer(predictors > threshold) == labels) / length(labels)
}