library(dplyr)

setwd("C:/Users/wxtof/Downloads")
data <- read.csv("telecom.csv")

summary(data)

data <- data[complete.cases(data),]

data$SeniorCitizen <- as.factor(ifelse(data$SeniorCitizen==1,"Yes","No"))
data$MultipleLines <- as.factor(ifelse(data$MultipleLines=="Yes","Yes","No"))

ls <- c("OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport", "StreamingTV", "StreamingMovies")
for (i in ls) {
  data[i] <- as.factor(ifelse(data[i]=="Yes","Yes","No"))
  
}


cat.var <- select_if(data, is.character)
data[,names(cat.var)] <- data.frame(lapply(data[,names(cat.var)], as.factor))


#data visualization
library(ggplot2)
p1 <- ggplot(data, aes(gender)) + geom_bar()
p2 <- ggplot(data, aes(Dependents)) + geom_bar() 
p3 <- ggplot(data, aes(Partner)) + geom_bar()
p4 <- ggplot(data, aes(SeniorCitizen)) + geom_bar() 

library(gridExtra)
grid.arrange(p1, p2, p3, p4,ncol=2)

p1 <- ggplot(data, aes(PhoneService)) + geom_bar()
p2 <- ggplot(data, aes(MultipleLines)) + geom_bar() 
p3 <- ggplot(data, aes(InternetService)) + geom_bar()
p4 <- ggplot(data, aes(OnlineSecurity)) + geom_bar() 

grid.arrange(p1, p2, p3, p4,ncol=2)

num.var <- select_if(data, is.numeric)
num.var.df <- data.frame(lapply(num.var, function(x) as.numeric(as.character(x))))

library(corrplot)
corrplot(cor(num.var.df), method="color", type="lower", tl.col="black" )

# logistic_regression
library(caret)

set.seed(1)

index <- createDataPartition(data$Churn, p=0.7, list=FALSE)

trainSet <- data[ index, ]
testSet <- data[-index, ]

predictors <- names(data)[-20]
outcomeName <- names(data)[20]

model_lr <- glm(Churn ~., data = trainSet,family="binomial")

summary(model_lr)

#Predicting using logit model
pred_lr <- predict(model_lr, testSet[, predictors], type="response")

pred_lr <- ifelse(pred_lr > 0.5,"Yes","No")
confusionMatrix(testSet$Churn,as.factor(pred_lr))

#decision tree
library(rpart)
library(rpart.plot)

model_dt <- rpart(Churn ~., data = trainSet, method="class")
rpart.plot(model_dt)

pred_dt <- predict(model_dt, testSet[, predictors])

pred_dt <- ifelse(pred_dt[,2] > 0.5,"Yes","No")
confusionMatrix(testSet$Churn,as.factor(pred_dt))

#random forest
library(randomForest)

fitControl <- trainControl(method = "cv", number=5, 
                     classProbs = TRUE, summaryFunction = twoClassSummary)

# train the model
model_rf <- train(trainSet[, predictors], trainSet[, outcomeName], method='rf',
                  trControl = fitControl)


model_rf$bestTune

model_rf <- randomForest(Churn ~., data = trainSet, 
                        ntree = 500, mtry = 2, 
                        importance = TRUE, proximity = TRUE)

pred_rf <- predict(model_rf, testSet)

confusionMatrix(testSet$Churn,as.factor(pred_rf))

varImpPlot(model_rf, sort=T, n.var = 10, 
           main = 'Top 10 important variables')

