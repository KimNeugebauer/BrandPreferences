### Loading data and libraries

BelkinElagoComplete <- 
  read.delim("C:/Users/kimne/Ubiqum/Module2/BelkinElagoComplete.csv")

View(BelkinElagoComplete)

BelkinData <- BelkinElagoComplete

library(caret)
library(mlbench)
library(C50)


### Renaming attributes 

names(BelkinData) <- 
  c("salary","age","education","car","zip","credit","brand")


### Data splitting

inTrain <- createDataPartition(y=BelkinData$brand, 
                               p=0.75, list=FALSE)
str(inTrain)

training <- BelkinData[inTrain,]
testing <- BelkinData[-inTrain,]



### Decision Tree Model #1

vars <- c("age","education","salary")

c50Fit <- C5.0(x = training[,vars], 
               y = training$brand,
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


### Alternate tuning grid ???

expand.grid(height = seq(60, 80, 5), weight = seq(100, 300, 100), 
            brand = c("Belkin","Elago"))