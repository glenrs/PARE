#' A calculateScoresAtThresholds Function
#' 
#' This function allows you to calculate scores at various thresholds
#' @param thresholdData datapoints main ??? width width of your graph height height of your graph
#' @keywords thresholdData, graphs
#' @export
#' @examples
#' calculateScoresAtThresholds(5,10,calculateAccuracy, 10)


calculateScoresAtThresholds <- function(labels, predictors, metricCalculationFunction=calculateAccuracy, numPermutations=10)
{
  checkPareInputs(labels, predictors)
  
  # Combine the labels and predictors so we can sort them together
  combined = cbind(as.integer(labels) - 1, predictors)
  combined = combined[order(labels),]
  
  # Then separate them back out
  classLabels = combined[,1]
  classPredictors = combined[,2]
  
  #thresholdStepSize = (max(classPredictors) - min(classPredictors)) / (length(classPredictors) + 1)
  #thresholds = seq(min(classPredictors), max(classPredictors), thresholdStepSize)
  thresholds = seq(0,1,.001)
  thresholds = thresholds[1:(length(thresholds)-1)]

  actualResultAtThresholds = calculateMetricAtThresholds(classLabels, classPredictors, thresholds, metricCalculationFunction)
  
  permutedMatrix = NULL
  for (i in 1:numPermutations)
  {
    set.seed(i)
    iPermuted = calculateMetricAtThresholds(sample(classLabels), classPredictors, thresholds, metricCalculationFunction)
    permutedMatrix <- rbind(permutedMatrix, iPermuted)
  }
  permutedResultAtThresholds <- apply(permutedMatrix, 2, mean)
  
  
  idealPredictors = sort(classPredictors)
 
  idealResultAtThresholds = calculateMetricAtThresholds(classLabels, idealPredictors, thresholds, metricCalculationFunction)
  
  wrongPredictors = sort(classPredictors, decreasing = TRUE)
  wrongResultAtThresholds = calculateMetricAtThresholds(classLabels, wrongPredictors, thresholds, metricCalculationFunction)
  
  pareScoreAtThresholds <- calculatePareScores(actualResultAtThresholds, permutedResultAtThresholds, idealResultAtThresholds, wrongResultAtThresholds)
  
  sensitivityAtThresholds = calculateMetricAtThresholds(classLabels, classPredictors, thresholds, calculateSensitivity)
  specificityAtThresholds = calculateMetricAtThresholds(classLabels, classPredictors, thresholds, calculateSpecificity)
  
  accuracyThresholdData <- data.frame(ScoreType="Maximal", Score=idealResultAtThresholds, Threshold=thresholds)
  accuracyThresholdData <- rbind(accuracyThresholdData, data.frame(ScoreType="Actual", Score=actualResultAtThresholds, Threshold=thresholds))
  accuracyThresholdData <- rbind(accuracyThresholdData, data.frame(ScoreType="Permuted", Score=permutedResultAtThresholds, Threshold=thresholds))
  accuracyThresholdData <- rbind(accuracyThresholdData, data.frame(ScoreType="Minimal", Score=wrongResultAtThresholds, Threshold=thresholds))
  accuracyThresholdData <- rbind(accuracyThresholdData, data.frame(ScoreType="PARE", Score=pareScoreAtThresholds, Threshold=thresholds))
  accuracyThresholdData <- rbind(accuracyThresholdData, data.frame(ScoreType="Sensitivity", Score=sensitivityAtThresholds, Threshold=thresholds))
  accuracyThresholdData <- rbind(accuracyThresholdData, data.frame(ScoreType="Specificity", Score=specificityAtThresholds, Threshold=thresholds))
 
  return(accuracyThresholdData)
}