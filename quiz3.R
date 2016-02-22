# Q1a

library(ElemStatLearn)
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
training <- segmentationOriginal[segmentationOriginal$Case=="Train",]
set.seed(125)
f1a <- train(Class ~ ., method="rpart", data=training)
library(rattle)
fancyRpartPlot(f1a$finalModel)

# a. TotalIntench2 = 23,000, FiberWidthCh1 = 10, PerimStatusCh1=2   ps
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10; VarIntenCh4 = 100  ws
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8; VarIntenCh4 = 100   ps
# d. FiberWidthCh1 = 8; VarIntenCh4 = 100; PerimStatusCh1=2         unknown

# Q3

library(pgmm)
data(olive)
olive = olive[,-1]
library(caret)
f3  <- train(Area ~ ., method="rpart", data=olive)
predict(f3, newdata = as.data.frame(t(colMeans(olive))))

# Q4

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
set.seed(13234)
f4 <- train(as.factor(chd) ~ age + alcohol + obesity + tobacco + typea + ldl, 
            method="glm", family="binomial", data=trainSA)
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
pTrain <- predict(f4)
pTrainNum <- as.numeric(pTrain) - 1
pTest <- predict(f4,newdata = testSA)
pTestNum <- as.numeric(pTest) - 1
trainSA$chd
round(missClass(pTestNum, testSA$chd), 2)
round(missClass(pTrainNum, trainSA$chd), 2)

# Q5.
library(ElemStatLearn)
library(caret)
data(vowel.train)
vowel.train$y <- as.factor(vowel.train$y)
data(vowel.test)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
f5 <- train(y ~ ., method="rf", data=vowel.train, importance=TRUE)
vi <- varImp(f5)
vi$n <- rownames(vi)
vi[order(-vi$Overall),]
varImpPlot(f5$finalModel)
