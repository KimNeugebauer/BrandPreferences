
### Introdcution to caret Tutorial - 
# Partial Least Squares Analysis of Belkin Elago Data


BelkinElagoComplete <- read.delim("C:/Users/kimne/Ubiqum/Module2/BelkinElagoComplete.csv")

View(BelkinElagoComplete)


# Libraries

library(caret)
library(mlbench)
library(ggplot2)
library(tidyverse)
library(dplyr)


# Renaming the data set and some variables

BelkinData <- BelkinElagoComplete

names(BelkinData) <- c("salary",
                       "age",
                       "education",
                       "car",
                       "zip",
                       "credit",
                       "brand")


### Data splitting into training and testing sets

inTrain <- createDataPartition(y=BelkinData$brand, p=0.75, list=FALSE)
str(inTrain)

training <- BelkinData[inTrain,]
testing <- BelkinData[-inTrain,]


# Defining the control function using repeated cross-validation

ctrl <- trainControl(method = "repeatedcv", repeats = 3)


# Training the PLS Model

plsFit <- train(brand~., data=training, method="pls", preProc=c("center","scale"), trControl=ctrl)

ggplot(plsFit)


### Using Metric ROC in the PLS Model

ctrl <- trainControl(method = "repeatedcv", repeats = 3,
                     classProbs=TRUE, summaryFunction=twoClassSummary)

plsFit <- train(brand~., data=training, method="pls", preProc=c("center","scale"), trControl=ctrl, metric="ROC")


# Predicting on the basis of our trained model

plsPred <- predict(plsFit, newdata=head(testing), type="prob")

ggplot(plsFit)


# Another attemp to gather train control, ROC curve and tune length into one model, just a try

plsFit <-
  train(
    brand ~ .,
    data = training,
    method = "pls",
    preProc = c("center", "scale"),
    tuneLength = 4,
    trControl = ctrl,
    metric = "ROC"
  )


### using Accuracy and Kappa as measurement tools

plsFit

ggplot(plsFit)


### Computing the Confusion Matrix for our predictions on brand choices

plsPred <- predict(plsFit, newdata=testing)
plsPred <- predict(plsFit, newdata=head(testing))

cmPls <- confusionMatrix(plsPred, testing$brand)

print(cmPls)


### Entropy and Info Gain

library(entropy)

entropy_2(BelkinData$education,BelkinData$brand)

ggplot(BelkinData, aes(education, fill = brand)) + geom_bar()

entropy(BelkinData$salary,method="ML")
entropy(BelkinData$credit,method="ML")
entropy(BelkinData$age,method="ML")
entropy(BelkinData$education,method="ML")

var_rank_info(BelkinData, "brand")


# Importance of variables

varImp(plsFit)


### Alternate tuning grid

expand.grid(height = seq(60, 80, 5), weight = seq(100, 300, 100), 
            brand = c("Belkin","Elago"))


