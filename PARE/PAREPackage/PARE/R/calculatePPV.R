#' A calculate PPV Function
#' 
#' This function allows you to calculate the PPV of your data
#' @param labels ??? a factor object predictors threshold
#' @keywords labels, predictors, PPV
#' @export
#' @examples
#' calculatePPV(2,3,5)

calculatePPV <- function(labels, predictors, threshold)
{
  if (length(predictors > threshold) == 0)
    return(0)
  
  length(intersect(which(predictors > threshold), which(labels == 1))) / sum(predictors > threshold)
}