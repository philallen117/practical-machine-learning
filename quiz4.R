# Quiz 4

# Q1

library(ElemStatLearn)
library(caret)
data(vowel.train)
vowel.train$y <- as.factor(vowel.train$y)
data(vowel.test)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)

f1rf <- train(y ~ ., method="rf", data=vowel.train)
f1b <- train(y ~ ., method="gbm", data=vowel.train)

pred_f1rf <- predict(f1rf, newdata=vowel.test)
acc_f1rf <- mean(pred_f1rf == vowel.test$y)

pred_f1b <- predict(f1b, newdata=vowel.test)
acc_f1b <- mean(pred_f1b == vowel.test$y)

agrees <- which(pred_f1b == pred_f1rf)
acc_agrees <- mean(pred_f1rf[agrees] == vowel.test[agrees,"y"])

# Q2

library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
set.seed(3433)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
f2rf <- train(diagnosis ~ ., method="rf", data=training)
f2gbm <- train(diagnosis ~ ., method="gbm", data=training)
f2lda <- train(diagnosis ~ ., method="lda", data=training)

p2rf <- predict(f2rf, newdata=testing)
p2gbm <- predict(f2gbm, newdata=testing)
p2lda <- predict(f2lda, newdata=testing)
preds <- data.frame(diagnosis=testing$diagnosis, p2rf, p2gbm, p2lda)
fstack <- train(diagnosis ~ ., method="rf", data=preds)
pstack <- predict(fstack, newdata = preds)

acc_p2rf <- mean(p2rf == testing$diagnosis)
acc_p2gbm <- mean(p2gbm == testing$diagnosis)
acc_p2lda <- mean(p2lda == testing$diagnosis)
acc_pstack <- mean(pstack == testing$diagnosis)
# Stack same as gbm, better than other two

# Q3.

library(caret)
library(AppliedPredictiveModeling)
data(concrete)
set.seed(3523)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)
f3 <- train(CompressiveStrength ~ ., method="lasso", data=concrete)
# cement

# Q4.

library(lubridate)
library(forecast)
setwd("C:/users/phil/repos/practical-machine-learning/")
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
f4 <- bats(tstrain)
tstest <- ts(testing$visitsTumblr)
p4 <- forecast(f4, h=nrow(testing), level=95)
mean(p4$lower < testing$visitsTumblr & testing$visitsTumblr < p4$upper) 
# 96%

# Q5.

library(caret)
library(AppliedPredictiveModeling)
library(e1071)
data(concrete)
set.seed(3523)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(325)
f5 = e1071::svm(CompressiveStrength ~ ., data=concrete)
p5 = predict(f5, newdata=testing)
forecast::accuracy(p5,testing$CompressiveStrength)
sqrt(mean((p5 - testing$CompressiveStrength)^2))
# 5.64
