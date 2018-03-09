# 
```{r}
 generateNumericScores = function(means, randomSeed=999, n=numRandomValues)
 {
   if (length(means) < 1)
     stop("Must provide at least one mean.")#
  set.seed(randomSeed)

  scores = NULL
  for (i in 1:length(means))
    scores = c(scores, rnorm(n / length(means), mean=means[i]))

  return(standardize(scores))
  }

generateNumericScoresImbalanced = function(means, randomSeed=0, n=numRandomValues)
{
  if (length(means) != 2)
    stop("Must provide exactly two means.")

  set.seed(randomSeed)

  scores = rnorm(n * imbalanceProportion, mean=means[1])
  scores = c(scores, rnorm(n * (1 - imbalanceProportion), mean=means[2]))

  return(standardize(scores))
}

standardize = function(x)
{
  (x - min(x)) / (max(x) - min(x))
}

numRandomValues = 1000
imbalanceProportion = 0.95

actual = factor(c(rep(0, numRandomValues / 2), rep(1, numRandomValues / 2)))
actualImbalanced = factor(c(rep(0, numRandomValues * imbalanceProportion), rep(1, numRandomValues * (1 - imbalanceProportion))))

randomScores <- generateNumericScores(0)
write.table(randomScores, file = "~/Downloads/randomScores.txt", row.names=FALSE, col.names=FALSE, quote=FALSE)
roundedRandomScores = round(randomScores)
write.table(roundedRandomScores, file = "~/Downloads/roundedRandomScores.txt", row.names=FALSE, col.names=FALSE, quote=FALSE)
mediumSignalScores = generateNumericScores(c(0, 1))
signalScores = generateNumericScores(c(0, 10))
oppositeScores = generateNumericScores(c(10, 0))
aFewGoodScores = standardize(c(rnorm(990), rnorm(10, mean=10)))

strongSignalDiscreteScores = as.integer(as.logical(as.numeric(as.vector(actual))))
oppositeBinaryScores = as.integer(!as.logical(as.numeric(as.vector(actual))))
singleValueScores = rep(as.integer(levels(actual)[2]), length(actual))
```

#Calibration Code CARET
testProbs <- data.frame(obs = actual,
                        randomScores = randomScores)

calibration(obs ~ randomScores, data = testProbs)

calPlotData <- calibration(obs ~ randomScores, data = testProbs)
calPlotData

xyplot(calPlotData, auto.key = list(columns = 2))

# mediumSignalScoresImbalanced = generateNumericScoresImbalanced(c(0, 1))
# signalScoresImbalanced = generateNumericScoresImbalanced(c(0, 10))
# oppositeScoresImbalanced = generateNumericScoresImbalanced(c(10, 0))
# 
# strongSignalDiscreteScoresImbalanced = as.integer(as.logical(as.numeric(as.vector(actualImbalanced))))
# oppositeBinaryScoresImbalanced = as.integer(!as.logical(as.numeric(as.vector(actualImbalanced))))
# ```
# 
# reliability.plot <- function(obs, pred, bins=10, scale=T) {
#   #  Plots a reliability chart and histogram of a set of predicitons from a classifier
#   #
#   # Args:
#   #   obs: Vector of true labels. Should be binary (0 or 1)
#   #   pred: Vector of predictions of each observation from the classifier. Should be real
#   #       number
#   #   bins: The number of bins to use in the reliability plot
#   #   scale: Scale the pred to be between 0 and 1 before creating reliability plot
#   require(plyr)
#   library(Hmisc)
#   
#   min.pred <- min(pred)
#   max.pred <- max(pred)
#   min.max.diff <- max.pred - min.pred
#   
#   if (scale) {
#     pred <- (pred - min.pred) / min.max.diff 
#   }
#   
#   bin.pred <- cut(pred, bins)
#   
#   k <- ldply(levels(bin.pred), function(x) {
#     idx <- x == bin.pred
#     c(sum(obs[idx]) / length(obs[idx]), mean(pred[idx]))
#   })
#   
#   is.nan.idx <- !is.nan(k$V2)
#   k <- k[is.nan.idx,]  
#   plot(k$V2, k$V1, xlim=c(0,1), ylim=c(0,1), xlab="Mean Prediction", ylab="Observed Fraction", col="red", type="o", main="Reliability Plot")
#   lines(c(0,1),c(0,1), col="grey")
#   subplot(hist(pred, xlab="", ylab="", main="", xlim=c(0,1), col="blue"), grconvertX(c(.8, 1), "npc"), grconvertY(c(0.08, .25), "npc"))
# }
# 
# reliability.plot(roundedRandomScores, randomScores)
# 
# #Platt scaling
# calib.data.frame <- data.frame(cbind(Y.calib, Y.calib.pred))
# colnames(calib.data.frame) <- c("y", "x")
# calib.model <- glm(y ~ x, calib.data.frame, family=binomial)
# calib.data.frame <- data.frame(Y.test.pred)
# colnames(calib.data.frame) <- c("x")
# Y.test.pred.calibrated <- predict(calib.model, newdata=calib.data.frame, type="response")


## CARET

library("caret")
data(mdrr)
mdrrDescr <- mdrrDescr[, -nearZeroVar(mdrrDescr)]
mdrrDescr <- mdrrDescr[, -findCorrelation(cor(mdrrDescr), .5)]

mdrrDescr
inTrain <- createDataPartition(mdrrClass)
trainX <- mdrrDescr[inTrain[[1]], ]
trainY <- mdrrClass[inTrain[[1]]]
testX <- mdrrDescr[-inTrain[[1]], ]
testY <- mdrrClass[-inTrain[[1]]]

library(MASS)

ldaFit <- lda(trainX, trainY)
qdaFit <- qda(trainX, trainY)

testProbs <- data.frame(obs = testY,
                        lda = predict(ldaFit, testX)$posterior[,1],
                        qda = predict(qdaFit, testX)$posterior[,1])

calibration(obs ~ lda + qda, data = testProbs)

calPlotData <- calibration(obs ~ lda + qda, data = testProbs)
calPlotData

xyplot(calPlotData, auto.key = list(columns = 2))

## End(Not run)


# PLATT

reliability.plot <- function(obs, pred, bins=10, scale=T) {
  #  Plots a reliability chart and histogram of a set of predicitons from a classifier
  #
  # Args:
  #   obs: Vector of true labels. Should be binary (0 or 1)
  #   pred: Vector of predictions of each observation from the classifier. Should be real
  #       number
  #   bins: The number of bins to use in the reliability plot
  #   scale: Scale the pred to be between 0 and 1 before creating reliability plot
  require(plyr)
  library(Hmisc)
  
  min.pred <- min(pred)
  max.pred <- max(pred)
  min.max.diff <- max.pred - min.pred
  
  if (scale) {
    pred <- (pred - min.pred) / min.max.diff 
  }
  
  bin.pred <- cut(pred, bins)
  
  k <- ldply(levels(bin.pred), function(x) {
    idx <- x == bin.pred
    c(sum(obs[idx]) / length(obs[idx]), mean(pred[idx]))
  })
  
  is.nan.idx <- !is.nan(k$V2)
  k <- k[is.nan.idx,]  
  plot(k$V2, k$V1, xlim=c(0,1), ylim=c(0,1), xlab="Mean Prediction", ylab="Observed Fraction", col="red", type="o", main="Reliability Plot")
  lines(c(0,1),c(0,1), col="grey")
  subplot(hist(pred, xlab="", ylab="", main="", xlim=c(0,1), col="blue"), grconvertX(c(.8, 1), "npc"), grconvertY(c(0.08, .25), "npc"))
}

reliability.plot(roundedRandomScores,randomScores)

#Can't get this to work
calib.data.frame <- data.frame(cbind(Y.calib, Y.calib.pred))
colnames(calib.data.frame) <- c("y", "x")
calib.model <- glm(y ~ x, calib.data.frame, family=binomial)
calib.data.frame <- data.frame(Y.test.pred)
colnames(calib.data.frame) <- c("x")
Y.test.pred.calibrated <- predict(calib.model, newdata=calib.data.frame, type="response")

#ISOTONIC REGRESSION

fit.isoreg <- function(iso, x0) 
{
  o = iso$o
  if (is.null(o)) 
    o = 1:length(x)
  x = iso$x[o]
  y = iso$yf
  ind = cut(x0, breaks = x, labels = FALSE, include.lowest = TRUE)
  min.x <- min(x)
  max.x <- max(x)
  adjusted.knots <- iso$iKnots[c(1, which(iso$yf[iso$iKnots] > 0))]
  fits = sapply(seq(along = x0), function(i) {
    j = ind[i]
    
    # Handles the case where unseen data is outside range of the training data
    if (is.na(j)) {
      if (x0[i] > max.x) j <- length(x)
      else if (x0[i] < min.x) j <- 1
    }
    
    # Find the upper and lower parts of the step
    upper.step.n <- min(which(adjusted.knots > j))
    upper.step <- adjusted.knots[upper.step.n]
    lower.step <- ifelse(upper.step.n==1, 1, adjusted.knots[upper.step.n -1] )
    
    # Pefrom a liner interpolation between the start and end of the step
    denom <- x[upper.step] - x[lower.step] 
    denom <- ifelse(denom == 0, 1, denom)
    val <- y[lower.step] + (y[upper.step] - y[lower.step]) * (x0[i] - x[lower.step]) / (denom)
    
    # Ensure we bound the probabilities to [0, 1]
    val <- ifelse(val > 1, max.x, val)
    val <- ifelse(val < 0, min.x, val)
    val <- ifelse(is.na(val), max.x, val) # Bit of a hack, NA when at right extreme of distribution
    val
  })
  fits
}

# Remove any duplicates
idx <- duplicated(Y.calib.pred)
Y.calib.pred.unique <- Y.calib.pred[!idx]
Y.calib.unique <- Y.calib[!idx]

iso.model <- isoreg(Y.calib.pred.unique, Y.calib.unique)

Y.test.pred.calibrated <- fit.isoreg(iso.model, Y.test.pred)

#PLATT #2

data(ToothGrowth) 
attach(ToothGrowth) 

# 1/0 coding 
dep          <- ifelse(supp == "VC", 1, 0) 
OneZeroModel <- glm(dep~len, family=binomial) 
OneZeroModel 
predict(OneZeroModel) 

# Platt coding 
dep2           <- ifelse(supp == "VC", 31/32, 1/32) 
plattCodeModel <- glm(dep2~len, family=binomial) 
plattCodeModel 
predict(plattCodeModel) 

compare        <- cbind(predict(OneZeroModel), predict(plattCodeModel)) 

plot(predict(OneZeroModel), predict(plattCodeModel))

