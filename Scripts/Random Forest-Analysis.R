### Random Forest Model


#Loading data and libraries

BelkinElagoComplete <- 
  read.delim("C:/Users/kimne/Ubiqum/Module2/BelkinElagoComplete.csv")

View(BelkinElagoComplete)

BelkinData <- BelkinElagoComplete

library(caret)
library(mlbench)
library(rpart)
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


### Random Forest, bagged trees, manually tuned

grid = expand.grid(.mtry = c(2,3)) 

ctrl <- trainControl(method = "oob" ,
                     search = "grid",
                     classProbs = TRUE
                     )

rfFit <- train(brand~., data=training, 
                method="rf", 
                preProc=c("center","scale"),
                tuneGrid = grid,
                trControl=ctrl
               )


rfFit
ggplot(rfFit)


### Importance of variables

varImp(rfFit)


### Prediction Model

rfPred <- predict(rfFit, newdata=testing, 
                   type="raw")  ## why raw? why does class not work ??

table(predict(rfFit))


### Confusion Metrix

cmrf <- confusionMatrix(rfPred, testing$brand)

print(cmrf)


