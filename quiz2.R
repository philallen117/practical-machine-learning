# week 2 quiz

# 1.

# 2.
library(caret)
library(AppliedPredictiveModeling)
data(concrete)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
library(Hmisc)
plot(mixtures$CompressiveStrength)
mixtures$CutCS <- cut2(mixtures$CompressiveStrength,g=5)
mixtures$idx <- 1:nrow(mixtures)
qplot(idx, FlyAsh, data=mixtures, colour=CutCS)
qplot(idx, Age, data=mixtures, colour=CutCS)
vars <- names(mixtures)[1:8]
for (v in vars) {
  qplot(idx, v, data=mixtures, colour=CutCS)
}

# 5.

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

cols <- grep("^IL",names(training),value=TRUE)
trainIL <- training[,cols]
pp <- preProcess(trainIL, method="pca", thresh=0.8)

# Same as 4 up to here. 7 PCs

trainILdiag <- cbind(diagnosis=training$diagnosis, trainIL)
mIL <- train(diagnosis ~ ., data=trainILdiag, method="glm")
predIL <- predict(mIL, newdata=testing) 
round(mean(predIL==testing$diagnosis), 2)
# confusionMatrix(predIL,testing$diagnosis) 
# .65 accuracy

# Pre-process training set
trainILP <- predict(pp,trainIL)
trainILPdiag <- cbind(diagnosis=training$diagnosis, trainILP)
mILP <- train(diagnosis ~ ., data=trainILPdiag, method="glm")
# Pre-process test set
testILP <- predict(pp,testing)
predILP <- predict(mILP, newdata=testILP)
round(mean(predILP==testing$diagnosis), 2)
# confusionMatrix(predILP,testing$diagnosis)
# .72 accuracy
