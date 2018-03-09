calculateNumCorrect <- function(labels, predictors, threshold)
{
  sum(as.integer(predictors > threshold) == labels)
}

checkPareInputs <- function(labels, predictors)
{
  if (!is.factor(labels))
    stop("The labels must be a factor object.")
  
  if (nlevels(labels) != 2)
    stop("The labels factor object must have exactly two levels.")
  
  if (length(intersect(c(0, 1), levels(labels))) != 2)
    stop("The factor levels of the labels object must be c(0, 1).")
  
  if (!is.numeric(predictors))
    stop("The predictors must be numeric.")
  
  if (length(predictors) != length(labels))
    stop("The length of the predictors and labels must be the same.")
  
  if (any(predictors > 1 | predictors < 0))
    stop("Predictions must fall between 0 and 1.")
}

calculatePareScores <- function(actualResultAtThresholds, permutedResultAtThresholds, idealResultAtThresholds, wrongResultAtThresholds)
{
  scoreAtThresholds <- NULL
  for (i in 1:length(actualResultAtThresholds))
  {
    actualResult <- actualResultAtThresholds[i]
    permutedResult <- permutedResultAtThresholds[i]
    idealResult <- idealResultAtThresholds[i]
    wrongResult <- wrongResultAtThresholds[i]
    
    actualDiff <- actualResult - permutedResult
    baselineDiff <- idealResult - permutedResult

    score = 0
    if (baselineDiff != 0)
    {
      score = actualDiff / baselineDiff
    }
    if (score < 0)
    {
      baselineDiff = permutedResult - wrongResult
      score = actualDiff / baselineDiff
    }
    
    scoreAtThresholds <- c(scoreAtThresholds, score)
  }
  
  return(scoreAtThresholds)
}

calculatePareScore <- function(accuracyThresholdData)
{
  return(mean(filter(accuracyThresholdData, ScoreType=="PARE")$Score))
}

getMainForPlot <- function(accuracyThresholdData, main)
{
  pareScore <- calculatePareScore(accuracyThresholdData)
  
  mainScore <- paste("(Score = ", format(pareScore, digits=3, nsmall=3), ")", sep="")
  
  if (main == "") {
    main = mainScore
  } else
  {
    main = paste(main, "\n", mainScore, sep="")
  }
  
  return(main)
}

calculateMetricAtThresholds <- function(labels, predictors, thresholds, metricCalculationFunction)
{
  sapply(thresholds, function(x) { metricCalculationFunction(labels, predictors, x) })
}
