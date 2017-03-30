library(caret)

QB<-read.csv("QBLogistic.csv",header = TRUE)

#separate train and test sets
set.seed(500)
index <- sample(1:nrow(QB),round(0.75*nrow(QB)))
train <- QB[index,]
test <- QB[-index,]

#get logistic model using train set
trainmodel <- glm(Class ~ SOS + Comp.Percent
                  + Att.TD + WR.DR + Starter
                  + Draft.Position + Age,
                  data = train, family="binomial")
summary(trainmodel)

#10 fold cv repeated 10 times
ctrl.1 <- trainControl(method = "repeatedcv",number = 10,
                       repeats = 10)

set.seed(500)
cv.1 <- train(Class ~ SOS + Comp.Percent
              + Att.TD + WR.DR + Starter
              + Draft.Position + Age,
              data = train, trControl=ctrl.1,method="glm", family="binomial")
cv.1


#get projections for test data and get labels
test$Project <- predict(trainmodel,test)
p <- function(x){
  if(x >= 0) y <- "Success"
  if(x < 0) y <- "Fail"
  return (y)
}
test$Predict <- sapply(test$Project,p)
test <- test[order(-test$Project),]
test$Pred <- (2.718^(test$Project))/(1+2.718^(test$Project))

testlabel <- test$Class

#confustion matrix for test data
confusionMatrix(testlabel,test$Predict)
