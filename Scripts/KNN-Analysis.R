### KNN

#Loading data and libraries

BelkinElagoComplete <- 
  read.delim("C:/Users/kimne/Ubiqum/Module2/BelkinElagoComplete.csv")

View(BelkinElagoComplete)

BelkinData <- BelkinElagoComplete

library(caret)
library(mlbench)


### Renaming attributes 

names(BelkinData) <- 
  c("salary","age","education","car","zip","credit","brand")


### Data splitting

inTrain <- createDataPartition(y=BelkinData$brand, 
                               p=0.75, list=FALSE)
str(inTrain)

training <- BelkinData[inTrain,]
testing <- BelkinData[-inTrain,]


### KNN model 

grid = expand.grid(k = c(3, 9, 16))

ctrl = trainControl(method = "cv",
                    number = 3,
                    search = "grid")

knnFit <- train(brand~ ., data = training,
                method = "knn",
                preProc = c("center", "scale"),
                tuneGrid = grid,
                #control = ctrl, ## not working any more.. -.-
                #tuneLength = 7
                )
knnFit

ggplot(knnFit)


## KNN Prediction

knnPred <- predict(knnFit, newdata = testing)

table(predict(knnFit))


### Confusion Metrix

cmknn <- confusionMatrix(knnPred, testing$brand)

print(cmknn)


### Importance of variables

varImp(knnFit)
