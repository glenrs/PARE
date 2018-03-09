#' A calculate Sensitivity Function
#' 
#' This function allows you to calculate the sensitivity of your data
#' @param labels ??? a factor object predictors threshold
#' @keywords labels, predictors, sensitivity
#' @export
#' @examples
#' calculateSensitivity(2,3,5)


calculateSensitivity <- function(labels, predictors, threshold)
{
  predictorsForPositiveLabels <- predictors[which(labels==1)]
  sum(predictorsForPositiveLabels > threshold) / sum(labels == 1)
}