#' A calculate NPV Function
#' 
#' This function allows you to calculate the NPV of your data
#' @param labels ??? a factor object predictors threshold
#' @keywords labels, predictors, NPV
#' @export
#' @examples
#' calculateNPV(2,3,5)

calculateNPV <- function(labels, predictors, threshold)
{
  if (length(predictors <= threshold) == 0)
    return(0)
  
  length(intersect(which(predictors <= threshold), which(labels == 0))) / sum(predictors <= threshold)
}