#' A plotPareScores Function
#' 
#' This function allows you to plot PARE scores using the ggplot2 package
#' @param thresholdData datapoints main ??? width width of your graph height height of your graph
#' @keywords thresholdData, graphs, datapoints
#' @export
#' @examples
#' plotPareScores(thresholdData, main = "", width=5, height=100)

plotPareScores <- function(accuracyThresholdData, metrics, ylab = "Accuracy", main="")
{
  plotData <- filter(accuracyThresholdData, ScoreType %in% metrics)
  
  print(ggplot(plotData, aes(x=Threshold, y=Score, group=ScoreType, colour=ScoreType)) + geom_line() + theme_bw() + theme(legend.key = element_blank(), legend.title=element_blank()) + ylab(ylab) + ggtitle(getMainForPlot(accuracyThresholdData, main)) + theme(plot.title = element_text(hjust = 0.5)))

}