#' A calculate Specificity Function
#' 
#' This function allows you to calculate the specificity of your data
#' @param labels ??? a factor object predictors threshold
#' @keywords labels, predictors, specificity
#' @export
#' @examples
#' calculateSpecificity(2,3,5)

calculateSpecificity <- function(labels, predictors, threshold)
{
  predictorsForNegativeLabels <- predictors[which(labels==0)]
  sum(predictorsForNegativeLabels <= threshold) / sum(labels == 0)
}