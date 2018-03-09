---
  title: "Performance Above Random Expectation"
author: "Stephen Piccolo"
date: "July 13, 2015"
output: html_document
---
  
  # Performance Above Random Expectation (PARE)
  
  ```{r}

suppressPackageStartupMessages(library(PARE))

#setwd("~/Dropbox/Publications/ROC_Alternative")
setwd("~/Dropbox/PARE/Archive")

dir.create("Figures", showWarnings = FALSE)

#replace width/height parameters with xlim ylim parameters, default to NULL
defaultWidth = NULL
defaultHeight = NULL
narrowWidth = NULL

#defaultWidth <- 8
#defaultHeight <- 4.5
#narrowWidth <- 4

saveFigure <- function(description1, description2, width=defaultWidth, height=defaultHeight)
{
  ggsave(paste("Figures/", gsub(" ", "_", description1), "__", description2, ".png", sep=""), width=width, height=height, units="in")
}
```

```{r}
generateNumericScores = function(means, randomSeed=999, n=numRandomValues)
{
  if (length(means) < 1)
    stop("Must provide at least one mean.")
  
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

randomScores = generateNumericScores(0)
mediumSignalScores = generateNumericScores(c(0, 1))
signalScores = generateNumericScores(c(0, 10))
oppositeScores = generateNumericScores(c(10, 0))
aFewGoodScores = standardize(c(rnorm(990), rnorm(10, mean=10)))

strongSignalDiscreteScores = as.integer(as.logical(as.numeric(as.vector(actual))))
oppositeBinaryScores = as.integer(!as.logical(as.numeric(as.vector(actual))))
singleValueScores = rep(as.integer(levels(actual)[2]), length(actual))

mediumSignalScoresImbalanced = generateNumericScoresImbalanced(c(0, 1))
signalScoresImbalanced = generateNumericScoresImbalanced(c(0, 10))
oppositeScoresImbalanced = generateNumericScoresImbalanced(c(10, 0))

strongSignalDiscreteScoresImbalanced = as.integer(as.logical(as.numeric(as.vector(actualImbalanced))))
oppositeBinaryScoresImbalanced = as.integer(!as.logical(as.numeric(as.vector(actualImbalanced))))
```

```{r}
# plotBarPlot <- function(values, main, width=defaultWidth)
# {
#   plotData <- data.frame(Scores=values)
# 
#   print(ggplot(plotData, aes(x=Scores)) + geom_bar() + theme_bw() + theme(legend.key = element_blank(), legend.title=element_blank()) + xlab("") + ylab("") + ggtitle(main))
#   saveFigure(main, "Barplot", width=width)
# }
# 
# plotHist <- function(values, main, width=defaultWidth)
# {
#   plotData <- data.frame(Scores=values)
# 
#   print(ggplot(plotData, aes(x=Scores)) + geom_histogram() + theme_bw() + theme(legend.key = element_blank(), legend.title=element_blank()) + xlab("") + ylab("") + ggtitle(main))
#   saveFigure(main, "Histogram", width=width)  
# }
# 
# plotBoxPlot <- function(scores, actual, main, width=defaultWidth)
# {
#   plotData <- data.frame(Scores=scores, Actual=actual)
# 
#   print(ggplot(plotData, aes(factor(Actual), Scores)) + geom_boxplot() + theme_bw() + theme(legend.key = element_blank(), legend.title=element_blank()) + xlab("") + ylab("") + ggtitle(main))
#   saveFigure(main, "BoxPlot", width=width)
# }
# 
# plotBarPlot(actual, main="Class Labels")
# plotBarPlot(actualImbalanced, main="Class Labels")
# 
# plotHist(randomScores, main="Random Scores")
# plotHist(mediumSignalScores, main="Medium Signal Scores")
# plotHist(signalScores, main="Signal Scores")
# plotHist(oppositeScores, main="Opposite Scores")
# plotHist(strongSignalDiscreteScores, main="Strong Signal Discrete Scores")
# plotHist(oppositeBinaryScores, main="Opposite Binary Scores")
# plotHist(aFewGoodScores, main="A Few Good Scores")
# 
# plotBoxPlot(randomScores, actual, main="Random Scores")
# plotBoxPlot(mediumSignalScores, actual, main="Medium Signal Scores")
# plotBoxPlot(signalScores, actual, main="Signal Scores")
# plotBoxPlot(oppositeScores, actual, main="Opposite Scores")
# plotBoxPlot(strongSignalDiscreteScores, actual, main="Strong Signal Discrete Scores")
# plotBoxPlot(oppositeBinaryScores, actual, main="Opposite Binary Scores")
# plotBoxPlot(aFewGoodScores, actual, main="A Few Good Scores", width=narrowWidth)
```

```{r}
# plotHist(mediumSignalScoresImbalanced, main="Medium Signal Scores Imbalanced")
# plotHist(signalScoresImbalanced, main="Signal Scores Imbalanced")
# plotHist(oppositeScoresImbalanced, main="Opposite Scores Imbalanced")
# plotHist(strongSignalDiscreteScoresImbalanced, main="Strong Signal Discrete Scores Imbalanced")
# plotHist(oppositeBinaryScoresImbalanced, main="Opposite Binary Scores Imbalanced")
# 
# plotBoxPlot(mediumSignalScoresImbalanced, actualImbalanced, main="Medium Signal Scores Imbalanced")
# plotBoxPlot(signalScoresImbalanced, actualImbalanced, main="Signal Scores Imbalanced")
# plotBoxPlot(oppositeScoresImbalanced, actualImbalanced, main="Opposite Scores Imbalanced")
# plotBoxPlot(strongSignalDiscreteScoresImbalanced, actualImbalanced, main="Strong Signal Discrete Scores Imbalanced")
# plotBoxPlot(oppositeBinaryScoresImbalanced, actualImbalanced, main="Opposite Binary Scores Imbalanced")
```

```{r}
# plotROC = function(actual, scores, main, width=defaultWidth)
# {
#   rocResult = roc(scores, actual)
#   aucValue = auc(rocResult)
#   
#   main2 = paste(main, "\n", "(AUC = ", format(aucValue, digits=3, nsmall=3), ")", sep="")
#   
#   plotData <- data.frame(TPR=rocResult$tpr, FPR=rocResult$fpr)
#   print(ggplot(plotData, aes(x=FPR, y=TPR)) + geom_abline(color="darkgrey", linetype="dashed") + geom_line() + theme_bw() + theme(legend.key = element_blank(), legend.title=element_blank()) + xlab("False positive rate") + ylab("True positive rate") + ggtitle(main2))
#   saveFigure(main, "ROC", width=width)
# }
# 
# plotROC(actual, randomScores, "Random Scores")
# plotROC(actual, mediumSignalScores, main="Medium Signal Scores")
# plotROC(actual, signalScores, main="Signal Scores")
# plotROC(actual, oppositeScores, main="Opposite Scores")
# plotROC(actual, aFewGoodScores, main="A Few Good Scores", width=narrowWidth)
# 
# plotROC(actual, strongSignalDiscreteScores, main="Strong Signal Discrete Scores")
# plotROC(actual, singleValueScores, main="Single Value Scores")
# plotROC(actual, oppositeBinaryScores, main="Opposite Binary Scores")
# 
# plotROC(actualImbalanced, randomScores, main="Random Scores Imbalanced")
# plotROC(actualImbalanced, mediumSignalScoresImbalanced, main="Medium Signal Scores Imbalanced")
# plotROC(actualImbalanced, signalScoresImbalanced, main="Signal Scores Imbalanced")
# plotROC(actualImbalanced, oppositeScoresImbalanced, main="Opposite Scores Imbalanced")
# plotROC(actualImbalanced, strongSignalDiscreteScoresImbalanced, main="Strong Signal Discrete Scores Imbalanced")
# plotROC(actualImbalanced, oppositeBinaryScoresImbalanced, main="Opposite Binary Scores Imbalanced")
```

```{r}

width <- defaultWidth
#width <- narrowWidth

pare(actual, randomScores, main="Random Scores", width=narrowWidth)
pare(actual, mediumSignalScores, main="Medium Signal Scores", width=width)
pare(actual, signalScores, main="Strong Signal Probabilistic Scores", width=narrowWidth)
pare(actual, oppositeScores, main="Opposite Scores", width=width)
pare(actual, aFewGoodScores, main="A Few Good Scores", width=defaultWidth)

pare(actual, strongSignalDiscreteScores, main="Strong Signal Discrete Scores", width=width)
pare(actual, oppositeBinaryScores, main="Opposite Binary Scores", width=width)
pare(actual, singleValueScores, main="Single Value Scores", width=width)


pare(actualImbalanced, randomScores, main="Random Scores Imbalanced", width=width)
pare(actualImbalanced, mediumSignalScoresImbalanced, main="Medium Signal Scores Imbalanced", width=narrowWidth)
pare(actualImbalanced, signalScoresImbalanced, main="Signal Scores Imbalanced", width=width)
pare(actualImbalanced, oppositeScoresImbalanced, main="Opposite Scores Imbalanced", width=width)
pare(actualImbalanced, strongSignalDiscreteScoresImbalanced, main="Strong Signal Discrete Scores Imbalanced", width=width)
pare(actualImbalanced, oppositeBinaryScoresImbalanced, main="Opposite Binary Scores Imbalanced", width=width)
```

```{r}
#FOR LOOP
#after loop make histogram
roundedscores=NULL
scores=NULL

plotHist <- function(values, main, width=defaultWidth)
{
  plotData <- data.frame(Scores=values)
  
  print(ggplot(plotData, aes(x=Scores)) + geom_histogram() + theme_bw() + theme(legend.key = element_blank(), legend.title=element_blank()) + xlab("") + ylab("") + ggtitle(main))
  saveFigure(main, "Histogram", width=width)  
}


mediumSignalScores = generateNumericScores(c(0, 1))
#mediumSignalScores = round(generateNumericScores(c(0,1)))

for(i in 1:100)
{
  score = pare(actual, mediumSignalScores, plot = FALSE)
  scores = c(scores, score)
}

plotHist(scores, main="Medium Signal Scores")


```


```{r}
compareAUCvPARE = function(meanOptions, iterations=20)
{
  aucScores = NULL
  pareScores = NULL
  
  for (i in 1:iterations)
  {
    message(paste("Iteration ", i, "/", iterations, sep=""))
    for (j in 1:length(meanOptions))
    {
      meanOption = meanOptions[j]
      predictors = generateNumericScores(c(0, meanOption), i*j)
      
      aucScores = c(aucScores, auc(roc(predictors, actual)))
      pareScore <-pare(actual, predictors, plot=FALSE)
      pareScores = c(pareScores, pareScore)
    }
  }
  
  hist(aucScores, main=paste("AUC:", format(mean(aucScores), digits=3, nsmall=3)))
  hist(pareScores, main=paste("PARE:", format(mean(pareScores), digits=3, nsmall=3)))
  
  plotData <- data.frame(AUC=aucScores, PARE=pareScores)
  print(ggplot(plotData, aes(x=AUC, y=PARE)) + geom_point() + theme_bw() + theme(legend.key = element_blank(), legend.title=element_blank()))
  saveFigure("AUC", "PARE")
}

#compareAUCvPARE(rep(0, 100))
#compareAUCvPARE(rnorm(5), iterations=5)
compareAUCvPARE(rnorm(100), iterations=100)
```


*****************************************************
  ** Stop here
*****************************************************
  
  ## Multiple classes
  
  #```{r}
  library(mlr)

data(iris)

task = makeClassifTask(id = "tutorial", data = iris, target = "Species")

n = getTaskSize(task)
train.set = seq(1, n, by = 2)
test.set = seq(2, n, by = 2)

task = makeClassifTask(id = "tutorial", data = iris, target = "Species")
lrn = makeLearner("classif.lda", predict.type = "prob")
mod = train(lrn, task, subset = train.set)
pred = predict(mod, task = task, subset = test.set)

performance(pred, measures = list(multiclass.auc))
#```

#```{r}
generateNumericScoresMultipleClasses = function(means, randomSeed=0, n=numRandomValues, defaultMean=0)
{
  if (length(means) < 1)
    stop("Must provide at least one mean.")
  
  set.seed(randomSeed)
  
  scores = NULL
  for (i in 1:length(means))
  {
    numValues = n / length(means)
    
    scoresThisClass = NULL
    for (j in 1:length(means))
    {
      if (i == j) {
        scoresThisClass = c(scoresThisClass, rnorm(numValues, mean=means[i]))
      } else {
        scoresThisClass = c(scoresThisClass, rnorm(numValues, mean=defaultMean))
      }
    }
    
    scores = cbind(scores, scoresThisClass)
  }
  
  scores = t(apply(scores, 1, function(x) { x / sum(x) }))
  
  colnames(scores) = 0:(length(means) - 1)
  
  return(scores)
}
#```

#```{r}
library(pROC)

plotMultiRoc = function(actual, scores, main)
{
  rocResult = multiclass.roc(actual, scores)
  print(rocResult)
  #  aucValue = auc(rocResult)
  
  #  par(mar=c(4.1, 4.6, 3.1, 0.6))
  #  plot(rocResult$fpr, rocResult$tpr, type="l", main=paste(main, "\n", "(AUC = ", format(aucValue, digits=3, nsmall=3), ")", sep=""), xlab="False positive rate", ylab="True positive rate")
  #  abline(0, 1, lty=2, col=2)
}

plotMultiRoc(actual4Classes, randomScores4Classes, main="Random Scores - 4 Classes")
#```

#```{r}
actual4Classes = factor(c(rep(0, numRandomValues / 4), rep(1, numRandomValues / 4), rep(2, numRandomValues / 4), rep(3, numRandomValues / 4)))
#actual4ClassesImbalanced = factor(c(rep(0, 10), rep(1, 10), rep(2, 490), rep(3, 490)))

randomScores4Classes = generateNumericScoresMultipleClasses(c(10, 10, 10, 10), n=numRandomValues, defaultMean=10)
mediumSignalScores4Classes = generateNumericScoresMultipleClasses(c(11, 11, 11, 11), n=numRandomValues, defaultMean=10)
signalScores4Classes = generateNumericScoresMultipleClasses(c(20, 20, 20, 20), n=numRandomValues, defaultMean=10)
oppositeScores4Classes = generateNumericScoresMultipleClasses(c(5, 5, 5, 5), n=numRandomValues, defaultMean=10)
signalVariesScores4Classes = generateNumericScoresMultipleClasses(c(10, 20, 10, 10), n=numRandomValues, defaultMean=10)
#```

#```{r}
pare(actual4Classes, randomScores4Classes, main="Random Scores - 4 Classes")
pare(actual4Classes, signalScores4Classes, main="Signal Scores - 4 Classes")
pare(actual4Classes, mediumSignalScores4Classes, main="Medium Signal Scores - 4 Classes")
pare(actual4Classes, oppositeScores4Classes, main="Opposite Scores - 4 Classes")
pare(actual4Classes, signalVariesScores4Classes, main="Signal Varies Scores - 4 Classes")
#```

#########################################################################
## Examine how mlr deals with multi-class data

#```{r}
library(mlr)

data = data.frame(a=rnorm(150), b=rnorm(150), Class=as.factor(c(rep(0, 50), rep(1, 50), rep(2, 50))))

task = makeClassifTask(id = "test", data = data, target = "Class")
lrn = makeLearner("classif.lda", predict.type = "prob")
rdesc = makeResampleDesc(method = "CV", stratify = TRUE)
r = resample(learner = lrn, task = task, resampling = rdesc, show.info = FALSE)
threshold = c("0"=0.33, "1"=0.33, "2"=0.33)
predictions = setThreshold(r$pred, threshold=threshold)
print(predictions$data)
print(predictions)

performance(r$pred, measures = list(acc, multiclass.auc))
#```
#########################################################################

## TODO

https://twitter.com/anshul/status/761117748126638084

Make a different version of PARE that gives a score for each individual sample. Compared to random chance for all patients of the same class, how accurate were the predictions for each sample? This is a more intuitive way to interpret the "A Few Good Scores" results shown in the PARE paper.  You could then take the average of these across all samples. If you don't include it in the grant, then write a paper based on it anyway.

One A Few Good Scores, why do the PARE scores at higher thresholds stay high? Should drop off, no?

Compare this to Cohen's Kappa (which seems to be interpreted the same way - but doesn't work for probabilistic classifiers?).
                               
                               Need some more scenarios where we can show the value of threshold selection.
                               
                               How does it work when there are only 5 distinct prediction values that are within a small range?
                               Could change it so the thresholds are actually the predictors rather than thresholds between them.
                               
                               It doesn't work right when using "truly ideal" and there is a class imbalance. Would be a nice selling point if you can tweak it to work.
                               Study this: http://web.cs.iastate.edu/~cs573x/Notes/hand-article.pdf
                               
                               Make it so it can find threshold that maximizes two metrics.
                               [Probably not needed.]
                               Maximize the minimum of the two metrics across all thresholds.
                               Can apply it to more than two metrics?
                               
                               Multiclass
                               What kind of output do you get when mlr is applied to multiclass data in mlr?
                               Seems like the only feasible way is to average across the classes.
                               Use the pROC package to plot multiclass.roc and compare against it.
                               Make sure the input class labels are zero/one.
                               
                               Break AUC:
                                 The ROC assumes two normal distributions. See if you can break it with something different.
                               Try a wide range of imbalance levels and see how PARE compares against AUC.
                               Smooth the permuted accuracy values.
                               2. How much difference do you see in the scores for correctly identifying 1/1000 correctly vs. 1/1000000 correctly for an extremely rare disease?
                               3. "Why not use log loss? That’s the metric that most of the Kaggle competitions moved to. It tests how well calibrated the probability statements of a model are in a way that neither 0/1 loss, squared error, or ROC curve metrics like mean precision don’t...The use of log on the error provides extreme punishments for being both confident and wrong. In the worst possible case, a single prediction that something is definitely true (1) when it is actually false will add infinite to your error score and make every other entry pointless. In Kaggle competitions, predictions are bounded away from the extremes by a small value in order to prevent this." (https://www.kaggle.com/wiki/LogarithmicLoss)
                               Report optimal cut point as max difference from permuted baseline?
                               Handle special case where you never see greater than 0.01 above permuted baseline.
                               Add Sensitivity and Specificity lines (grey, dotted) to the plot.
                               
                               ## FUTURE
                               Multiclass
                               Read these three papers in Evernote:
                                 Receiver Operating Characteristic Analysis:A Primer
                               A Simple Generalisation of the Area Under the ROCCurve for Multiple Class Classification Problems
                               Statistics review 13: Receiver operating characteristic curves
                               Add option to change the "ideal" result so that it actually is the ideal (0 or 1 probabilities).
                               * In theory, this is what you do with accuracy.
                               * Makes it so you can compare models more fairly.
                               http://andrewgelman.com/2016/01/30/evaluating-models-with-predictive-accuracy/
                                 Provide a parameter that enables the user to limit the PARE calculation to a specific range of thresholds and demonstrate what this gives you.
                               Use bootstrap subsampling to calculate confidence intervals. Should see wider intervals for smaller data sets.
                               Merge the two plots I currently have and use ggplot2 to plot the combined plot.
                               Use plotThreshVsPerf in mlr to create plots.
                               
                               Add more error checking of input parameters on the pare function and related functions
                               Scale input predictor values between 0 and 1 if not already in that range?
                               Let them specify the number of thresholds and/or threshold step size as a parameter.
                               
                               "A systematic analysis of performance measures for classification tasks" in Evernote.
                               * AUC "can also be viewed as a linear transformation of Youden Index"
                               * What is the Youden index and how is it calculated?
                               * Invariance property (switching around the confusion matrix). Can we say that PARE meets this better than accuracy?
                               
                               Try different sample sizes: 100000, 1000, 100, 10
                               Read this and make notes within this notebook: http://web.cs.iastate.edu/~cs573x/Notes/hand-article.pdf
                               Can you deal with the the problem of lines crossing.
                               http://papers.ssrn.com/sol3/papers.cfm?abstract_id=1620718
                               Address the question that people ask, "At what level of accuracy do you get excited?"
                               Emphasize how you can use this method to quantify whether performance is better than clinical/baseline predictors.
                               Can you apply it to multilabel classification problems? That's a new angle.
                               Can you apply it to regression problems?
                               
                               
                               
                               ## Notes
                               
                               A key contribution would be that it generalizes beyond two classes.
                               
                               Its roots in signal processing make it more difficult for people to understand.
                               
                               Here's a list of classification measures: https://mlr-org.github.io/mlr-tutorial/tutorial/release/html/measures/index.html
                               
                               One non-intuitive thing about ROC plots is that it is varying the threshold along an axis, but it is does not show that threshold explicitly on the plot. It's also not intuitive that an AUC value of 0.5 is what you expect by chance. Also, it's difficult to calculate the AUC. And it doesn't work for multi-class problems. Also sensitivity and specificity are difficult for many people to understand.
                               
                               What is positive about this metric is that it is robust to class imbalance and is used widely and relatively easy to interpret because it can be related loosely back to accuracy. And that it doesn't force you to choose one specific threshold.
                               
                               What this could also show you is which is the optimal threshold to use in clinical settings.
                               
                               To get this into a top journal (Nature Methods), you could write this as a tutorial/review article and add your own metric at the end.
                               
                               These curves seem to have two main purposes: 1) assessing the balance between sensitivity and specificity and 2) comparing classifiers. Need to address both of these needs?

Develop a tool in Python (http://scikit-learn.org/stable/modules/generated/sklearn.metrics.auc.html) and/or in R (http://stackoverflow.com/questions/4954507/calculate-the-area-under-a-curve-in-r).

See info here: http://en.wikipedia.org/wiki/Receiver_operating_characteristic.

Look at paper entitled, "Union–intersection permutation solution for two-sample
equivalence testing."

Summary of performance measures in machine learning: http://www.cs.cornell.edu/courses/cs578/2003fa/performance_measures.pdf

Nicolas Robine @notSoJunkDNA

KP: Side point: Harmonic mean of precision and recall (Fmax statistics) is better than AUC in many genomics application… #5points

https://twitter.com/notSoJunkDNA/status/580732327149596672

Harmonic mean of precision/recall is described in http://www.cs.cornell.edu/courses/cs578/2003fa/performance_measures.pdf

https://rocr.bioinf.mpi-sb.mpg.de/
  
  Has an implementation of accuracy threshold plot. Many other metrics, too.

The precision-recall curve is an alternative. But it's not really better. This presentation provides some theoretical background: http://www.ke.tu-darmstadt.de/lehre/archiv/ws0708/ml-sem/Folien/Wen_Zhang.pdf

Here's another alternative along with some more theory: http://link.springer.com/article/10.1007%2Fs10994-009-5119-5

R package described here: http://www.hmeasure.net

Claims that AUC is equivalent to the "Gini coefficient".

"For example, if ROC curves cross then it is possible that one curve has a larger AUC (and so is apparently better) even though the alternative may show superior performance over almost the entire range of values of the classification threshold (defined below) for which the curve will be used."

"a much more fundamental weakness of the AUC which appears not to have been previously recognised. This is that, as we show below, the AUC is equivalent to measuring the performance of classification rules using metrics which depend on the rules being measured. In particular, instead of regarding the relative severities of different kinds of misclassifications (i.e., misclassifying a class 0 object as class 1, and a class 1 as class 0) as the same for different classifiers, the AUC takes these relative severities themselves to depend on the classifier being measured. This is, of course, nonsensical, since the relative severities, even if one does not know them precisely, are independent of the classifiers themselves and must be determined by factors describing the problem external to the classifiers. It is as if one chose to compare the heights of two people using rulers in which the basic units of measurement themselves depended on the heights."

Here's another one: http://users.dsic.upv.es/~flip/ROCAI2004/papers/03-vfinal-Drummond_and_Holte-A4.pdf They emphasize confidence intervals, which is important and should be included in our tool (bootstrap approach?).

Another: http://www.nssl.noaa.gov/users/brooks/public_html/feda/papers/Drummond%20and%20Holte.pdf

http://blog.markus-breitenbach.com/2010/02/16/alternative-measures-to-the-auc-for-rare-event-prognostic-models/

Look at this book: https://books.google.com/books?id=sU1YBAAAQBAJ&pg=PT44&lpg=PT44&dq=roc+curve+alternative&source=bl&ots=4bTnwHE4-f&sig=lcigp9xNAL2NERlxo3U2-BXzXO8&hl=en&sa=X&ei=0SPNVIvlBs_8oQTrg4GIBQ&ved=0CDMQ6AEwBDgU#v=onepage&q=roc%20curve%20alternative&f=false

http://www.hmeasure.net/

http://www.igi-global.com/article/alternative-approach-evaluating-binormal-roc/80236

http://www.jstor.org/discover/10.2307/3702911?sid=21105221364251&uid=2&uid=4&uid=3739256&uid=3739808

http://hunch.net/?p=21

Use the comparison methodology described here?: http://webdocs.cs.ualberta.ca/~ozlem/papers/TR2009.pdf

Another possibility is to come up with an alternative to survival analysis: http://errorstatistics.com/2013/04/19/stephen-senn-when-relevance-is-irrelevant/