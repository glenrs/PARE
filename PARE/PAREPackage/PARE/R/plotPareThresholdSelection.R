#' A plotPareThresholdSelection Function
#' 
#' This function allows you to plot PARE Threshold Selection ?? using the ggplot2 package
#' @param thresholdData datapoints main ??? width width of your graph height height of your graph
#' @keywords thresholdData, graphs
#' @export
#' @examples
#' plotPareScores(thresholdData, main = "", width=5, height=100)


plotPareThresholdSelection <- function(accuracyThresholdData, main="")
{
  
  accuracyPARE <- filter(accuracyThresholdData, ScoreType=="PARE")

  accuracyPARE$ScoreType <- "Accuracy"

  print(ggplot(accuracyPARE, aes(x=Threshold, y=Score, group=ScoreType, colour=ScoreType)) + geom_line() + scale_colour_manual(values=c("black", "blue", "orange")) + theme_bw() + theme(legend.position="none") + ylab("PARE") + ggtitle(getMainForPlot(accuracyThresholdData, main)) + scale_y_continuous() + geom_hline(yintercept=0, color="red", linetype="dashed") + theme(plot.title = element_text(hjust = 0.5)))

}
