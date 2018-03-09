#' A calculate Fscore Function
#' 
#' This function allows you to calculate the Fscore of your data
#' @param labels ??? a factor object predictors threshold
#' @keywords labels, predictors, Fscore
#' @export
#' @examples
#' calculateFscore(2,3,5)

calculateFscore <- function(labels, predictors, threshold)
{
  precision <- calculatePPV(labels, predictors, threshold)
  recall <- calculateSensitivity(labels, predictors, threshold)
  
  (2 * (precision * recall)) / (precision + recall)
}