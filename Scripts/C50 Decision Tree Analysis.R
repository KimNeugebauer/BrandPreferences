### Loading data and libraries

BelkinElagoComplete <- 
  read.delim("C:/Users/kimne/Ubiqum/Module2/BelkinElagoComplete.csv")

View(BelkinElagoComplete)

BelkinData <- BelkinElagoComplete

library(caret)
library(mlbench)
library(C50)
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



### Decision Tree Model #1

vars <- c("age","education","salary")

c50Fit <- C5.0(x = training[,vars], 
               y = training$brand,
               tuneLength = 4,
               mtry = 2,
               control = C5.0Control())

summary(c50Fit)

plot(c50Fit)

table(predict(c50Fit,testing[,vars]))


## Prediction Model

c50Pred <- predict(c50Fit, 
                   newdata = testing[1:10,vars])


## Importance of variables

C5imp(c50Fit, metric="splits")


### Confusion Metrix

confusionMatrix(c50Pred, testing$brand)




### Decision Tree Model #2 - Gradient boosting

c50_boost <- C5.0(x=training[,vars],
                  y = training$brand, 
                  trial=  5, 
                  control = C5.0Control())

summary(c50_boost)

plot(c50_boost)

table(predict(c50_boost, testing[,vars]))


### Prediction Model, different variations

c50Pred_boost <- predict(c50_boost, 
                         newdata = testing[1:4,vars])

c50Pred_boost <- predict(c50_boost, 
                         newdata = testing[1:2500,vars],
                         type="class")

c50Pred_boost <- predict(c50_boost, 
                         newdata = testing[1:10,vars],
                         type="prob")


## Importance of variables

C5imp(c50_boost, metric="splits") 


### Confusion Metrix

confusionMatrix(c50Pred_boost, testing$brand)
