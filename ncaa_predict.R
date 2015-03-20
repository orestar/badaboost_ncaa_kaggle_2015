## ----eval=FALSE,echo=FALSE-----------------------------------------------
#  opts_chunk$set(eval=FALSE,echo=TRUE,message=FALSE,warning=FALSE,error=FALSE)
#  

## ------------------------------------------------------------------------
#  library(caret)
#  library(DAMisc)
#  library(splines)
#  library(caretEnsemble)
#  library(foreign)
#  library(sna)
#  library(network)
#  library(psych)
#  library(doSNOW)
#  library(parallel)

## ------------------------------------------------------------------------
#  require('devtools')
#  install_github('zachmayer/kaggleNCAA')
#  require(kaggleNCAA)

## ------------------------------------------------------------------------
#   require(devtools)
#   install_github('trcook/tmisc',subdir='tmisc')
#   .boxcar_token<-c("TOKEN_HERE")
#   library(tmisc)

## ------------------------------------------------------------------------
#  ncluster<-4
#  

## ------------------------------------------------------------------------
#  setwd("C:/Users/Thomas/Desktop/NCAA/")

## ------------------------------------------------------------------------
#  ivalidate<-read.csv("./ivalidate_pt1.csv")
#  start<-Sys.time()
#  if(ncluster>0){
#  cl<-makeCluster(ncluster)
#  registerDoSNOW(cl)
#  }

## ------------------------------------------------------------------------
#  my_control <- trainControl(
#    method='repeatedcv',
#    repeats=5,
#    savePredictions=TRUE,
#    classProbs=TRUE,
#    summaryFunction=twoClassSummary
#  )

## ------------------------------------------------------------------------
#  models <- caretList(
#    win~., data=ivalidate[,grepl("win|survscores|powscore|rank|winper|rpi|sos|ncf|orank", names(ivalidate))],
#    trControl=my_control,
#    methodList=c('bagFDA', 'nnet', 'ada', 'bayesglm', 'svmPoly', 'rf', 'knn', 'svmLinear', 'gbm')#'knn', , 'qrnn', 'svmPoly', 'AdaBag'
#  )

## ------------------------------------------------------------------------
#  stack <- caretStack(models, method='glm')
#  greedy <- caretEnsemble(models, iter=1000L)
#  

## ------------------------------------------------------------------------
#  if(ncluster>0){
#  stopCluster(cl)
#  }

## ------------------------------------------------------------------------
#  end<-Sys.time()
#  boxcar_notify(token = .boxcar_token,body = paste("time taken:",c(start-end)),title = "Training Done")

## ------------------------------------------------------------------------
#  ivalidate<-read.csv("./ivalidate_pt2.csv")

## ------------------------------------------------------------------------
#  preds <- predict(stack, type="prob", newdata = ivalidate[ ,grepl("win|survscores|powscore|rank|winper|rpi|sos|ncf|orank", names(ivalidate))])[,1]
#  df <- data.frame(preds=preds[which(ivalidate$daynum>135)], realscore=ivalidate$scorediff[which(ivalidate$daynum>135)], season=ivalidate$season[which(ivalidate$daynum>135)])
#  qplot(preds, realscore, data=df, xlab="Prediction", ylab="Real Margin") + geom_smooth(method="loess")
#  df$win <- 1*(df$realscore>0)
#  df$pwin <- 1*(df$preds>=.5)
#  logloss <- sum((df$win*log(df$preds) + (1-df$win)*log(1-df$preds))  * (1/nrow(df)) ); logloss
#  accuracy <- sum(df$win==df$pwin)/nrow(df) #Make 65% accuracy
#  

## ------------------------------------------------------------------------
#  CappedBinomialDeviance <- function(a, p) {
#    if (length(a) !=  length(p)) stop("Actual and Predicted need to be equal lengths!")
#    p_capped <- pmin(0.99, p)
#    p_capped <- pmax(0.01, p_capped)
#    -sum(a * log(p_capped) + (1 - a) * log(1 - p_capped)) / length(a)
#  }
#  CappedBinomialDeviance(df$win, df$preds)

## ------------------------------------------------------------------------
#  ivalidate<-read.csv("./ivalidate_pt3.csv")

## ------------------------------------------------------------------------
#  start<-Sys.time()
#  if(ncluster>0){
#  cl<-makeCluster(ncluster)
#  registerDoSNOW(cl)
#  }
#  

## ------------------------------------------------------------------------
#  my_control <- trainControl(
#    method='repeatedcv',
#    repeats=5,
#    savePredictions=TRUE,
#    classProbs=TRUE,
#    summaryFunction=twoClassSummary
#  )

## ------------------------------------------------------------------------
#  models <- caretList(
#    win~., data=ivalidate[,grepl("win|survscores|powscore|rank|winper|rpi|sos|ncf", names(ivalidate))],
#    trControl=my_control,
#    methodList=c('bagFDA', 'nnet', 'ada', 'bayesglm', 'svmPoly', 'rf', 'knn', 'svmLinear', 'gbm')#'knn', , 'qrnn', 'svmPoly', 'AdaBag'
#  )
#  

## ------------------------------------------------------------------------
#  stack <- caretStack(models, method='glm')
#  greedy <- caretEnsemble(models, iter=1000L)
#  

## ------------------------------------------------------------------------
#  if(ncluster>0){
#  stopCluster(cl)
#  }
#  

## ------------------------------------------------------------------------
#  end<-Sys.time()
#  boxcar_notify(token = .boxcar_token,body = paste("time taken:",c(start-end)),title = "Final Training Done")

## ------------------------------------------------------------------------
#  ivalidate<-read.csv("./ivalidate_pt4.csv")
#  df2<-read.csv("./df2.csv")
#  df<-read.csv("./df.csv")

## ------------------------------------------------------------------------
#  preds <- predict(stack, type="prob", newdata = df2[,grepl("win|survscores|powscore|rank|winper|rpi|sos|ncf|orank", names(df2))])[,2]

## ------------------------------------------------------------------------
#  finaldf <- data.frame(id=df$id, pred=1-preds)
#  write.csv(finaldf, "./stage2_n2.csv", row.names=F)

## ------------------------------------------------------------------------
#  d1 <- read.csv("./kaggle_submission_public.csv")
#  d2 <- read.csv("./stage2_n2.csv"); names(d2)[2] <- "mypred"
#  dat <- merge(d1, d2, by="id")
#  qplot(mypred, pred, data=dat)

## ------------------------------------------------------------------------
#  dat<-parseBracket("./stage2_n2.csv")
#  setnames(dat,names(dat),c("season", "team_2", "team_1", "pred"))
#  bracket <- walkTourney(dat, year=2015)
#  printableBracket(bracket)

