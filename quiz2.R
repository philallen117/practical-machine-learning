# week 2 quiz

# 1.
library(caret)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]
# looks good to me!

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

#3.
library(AppliedPredictiveModeling)
data(concrete)
hist(concrete$Superplasticizer)
hist(log(1 + concrete$Superplasticizer))
nz <- concrete$Superplasticizer[concrete$Superplasticizer > 0]
hist(nz)
hist(log(nz))
# A lot around zero, so log transformation would not make it look normal

# 4.
library(caret)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
set.seed(3433)
adData = data.frame(diagnosis, predictors)
inTrain4 = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training4 = adData[ inTrain4,]
testing4 = adData[-inTrain4,]
cols <- grep("^IL", names(training4), value=TRUE)
trainIL <- training4[,cols]
pp <- preProcess(trainIL, method="pca", thresh=0.9)
# 9 components

# 5.
library(caret)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
set.seed(3433)
adData = data.frame(diagnosis, predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

cols <- grep("^IL", names(training), value=TRUE)
trainIL <- training[,cols]
#put diagnosis back
trainILdiag <- cbind(diagnosis=training$diagnosis, trainIL) 
mIL <- train(diagnosis ~ ., data=trainILdiag, method="glm")
predIL <- predict(mIL, newdata=testing) 
round(mean(predIL==testing$diagnosis), 2)
# .65 accuracy

# Find PCAs
pp <- preProcess(trainIL, method="pca", thresh=0.9)
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
