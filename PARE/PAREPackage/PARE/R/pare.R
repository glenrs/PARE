#' A pare Function
#' 
#' This function allows you to plot PARE scores using the ggplot2 package
#' @param labels ??? a factor object predictors numeric??? plot default=TRUE to defaultWidth height height of graph defaults to defaultHeight
#' @keywords labels, predictors, accuracy
#' @export
#' @examples
#' pare(4,5,"main",FALSE, width=5,height=100) ???


pare <- function(labels, predictors, main="", plot=TRUE)
{
  accuracyThresholdData <- calculateScoresAtThresholds(labels, predictors, calculateAccuracy)
  
  if (plot)
  {
    plotPareScores(accuracyThresholdData, c("Actual", "Maximal", "Minimal", "Permuted"), "Accuracy", main)

    accuracyThresholdData2 <- accuracyThresholdData
    accuracyThresholdData2$ScoreType <- as.character(accuracyThresholdData2$ScoreType)
    accuracyThresholdData2$ScoreType[which(accuracyThresholdData2$ScoreType=="Actual")] <- "Accuracy"
    accuracyThresholdData2$ScoreType <- as.factor(accuracyThresholdData2$ScoreType)
    plotPareScores(accuracyThresholdData2, c("Accuracy", "Sensitivity", "Specificity"), "Performance", main)

    plotPareThresholdSelection(accuracyThresholdData, main)
  }
  
  return(calculatePareScore(accuracyThresholdData))
}

