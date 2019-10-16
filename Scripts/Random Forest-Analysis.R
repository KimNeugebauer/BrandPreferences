### Random Forest Model


#Loading data and libraries

BelkinElagoComplete <- 
  read.delim("C:/Users/kimne/Ubiqum/Module2/BelkinElagoComplete.csv")

View(BelkinElagoComplete)

BelkinData <- BelkinElagoComplete

library(caret)
library(mlbench)
library(rpart)


### Renaming attributes 

names(BelkinData) <- 
  c("salary","age","education","car","zip","credit","brand")


### Data splitting

inTrain <- createDataPartition(y=BelkinData$brand, 
                               p=0.75, list=FALSE)
str(inTrain)

training <- BelkinData[inTrain,]
testing <- BelkinData[-inTrain,]


### Random Forest, bagged trees

grid = expand.grid(mtry = c(3, 4, 8))  # why this shitty mtry ??

ctrl <- trainControl(method = "oob", 
                     search = "grid",
                     classProbs = TRUE
                     )

rfFit <- train(brand~., data=training, 
                method="rf", 
                preProc=c("center","scale"),
                tuneGrid = grid,
                #tuneLength = 3, ## what does it tell me here?
                trControl=ctrl,
               )
rfFit
ggplot(rfFit)



### Prediction Model

rfPred <- predict(rfFit, newdata=testing, 
                   type="raw")  ## why raw? why does class not work ??

table(predict(rfFit))


### Confusion Metrix

cmrf <- confusionMatrix(rfPred, testing$brand)

print(cmrf)


### Importance of variables

varImp(rfFit)


