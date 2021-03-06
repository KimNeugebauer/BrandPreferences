
### Introdcution to caret Tutorial - Pls Analysis of Belkin Data

#Loading data and libraries

BelkinElagoComplete <- 
  read.delim("C:/Users/kimne/Ubiqum/Module2/BelkinElagoComplete.csv")

View(BelkinElagoComplete)

BelkinData <- BelkinElagoComplete

library(caret)
library(mlbench)
library(dplyr)


### Renaming attributes 

names(BelkinData) <- 
  c("salary","age","education","car","zip","credit","brand")



### Selecting subset of arrtributes, pre-processing

BelkinSmall <- BelkinData %>% select("salary","age","education","brand")

as.factor(BelkinSmall$brand)
as.factor(BelkinSmall$education)


### Data splitting

inTrain <- createDataPartition(y=BelkinSmall$brand, 
                               p=0.75, list=FALSE)
str(inTrain)

training <- BelkinSmall[inTrain,]
testing <- BelkinSmall[-inTrain,]


### PLS Model, 10 fold cross validation

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10,
                     repeats = 3)

plsFit <- train(brand~., data=training, 
                method="pls", 
                preProc=c("center","scale"),
                tuneLength = 4,
                trControl=ctrl)
ggplot(plsFit)


### PLS model using Metric = ROC

ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3,
                     number = 10,
                     classProbs=TRUE, 
                     summaryFunction=twoClassSummary)

plsFit <- train(brand~., data=training, 
                method="pls", 
                preProc=c("center","scale"),
                tuneLength = 4,
                trControl=ctrl, 
                metric="ROC")

ggplot(plsFit)

plsFit


### Importance of variables

varImp(plsFit)



### Predictions using probabilities

plsPred <- predict(plsFit, newdata=testing, 
        type="prob")   ## in this option the confusion metric does not work



### Complete PLS Model with Accuracy and Kappa, no probabilities

ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3)

plsFit <- train(brand~ ., data = training,
                method = "pls",
                preProc = c("center", "scale"),
                tuneLength = 3,
                trControl = ctrl)

ggplot(plsFit)

plsPred <- predict(plsFit, newdata = testing)


### Confusion Metrix

cmPls <- confusionMatrix(plsPred, testing$brand)

print(cmPls)


### Entropy and Info Gain

library(entropy)

entropy_2(BelkinData$education,BelkinData$brand)

# to check, see: ggplot(BelkinData, aes(education, fill = brand)) + geom_bar()


entropy(BelkinData$salary,method="ML")

entropy(BelkinData$credit,method="ML")

entropy(BelkinData$age,method="ML")

entropy(BelkinData$education,method="ML")

