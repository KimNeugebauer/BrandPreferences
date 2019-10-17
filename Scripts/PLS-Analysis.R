
### Introdcution to caret Tutorial - Pls Analysis of Belkin Data

BelkinElagoComplete <- read.delim("C:/Users/kimne/Ubiqum/Module2/BelkinElagoComplete.csv")
View(BelkinElagoComplete)

BelkinData <- BelkinElagoComplete

names(BelkinData) <- c("salary","age","education","car","zip","credit","brand")

library(caret)
library(mlbench)

### Data splitting

inTrain <- createDataPartition(y=BelkinData$brand, p=0.75, list=FALSE)
str(inTrain)

training <- BelkinData[inTrain,]
testing <- BelkinData[-inTrain,]


plsFit <- train(brand~., data=training, method="pls", preProc=c("center","scale"))

ctrl <- trainControl(method = "repeatedcv", repeats = 3)

plsFit <- train(brand~., data=training, method="pls", preProc=c("center","scale"),
              trControl=ctrl)
ggplot(plsFit)


### using Metric ROC

ctrl <- trainControl(method = "repeatedcv", repeats = 3,
                     classProbs=TRUE, summaryFunction=twoClassSummary)

plsFit <- train(brand~., data=training, method="pls", preProc=c("center","scale"),
              trControl=ctrl, metric="ROC")

predict(plsFit, newdata=head(testing), type="prob")

ggplot(plsFit)


plsPred <- predict(plsFit, newdata=head(testing), type="prob")

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

ctrl <- trainControl(method = "repeatedcv", repeats = 3)

plsFit <-
  train(
    brand ~ .,
    data = training,
    method = "pls",
    preProc = c("center", "scale"),
    tuneLength = 10,
    trControl = ctrl
  )

plsFit
ggplot(plsFit)

plsPred <- predict(plsFit, newdata=head(testing))


### Confusion Metrix
plsPred <- predict(plsFit, newdata=testing)

cmPls <- confusionMatrix(plsPred, testing$brand)

print(cmPls)


### Entropy and Info Gain / Importance of variables

library(entropy)

entropy_2(BelkinData$education,BelkinData$brand)
ggplot(BelkinData, aes(education, fill = brand)) + geom_bar()

entropy(BelkinData$salary,method="ML")
entropy(BelkinData$credit,method="ML")
entropy(BelkinData$age,method="ML")
entropy(BelkinData$education,method="ML")

var_rank_info(BelkinData, "brand")

varImp(plsFit)

### Alternate tuning grid

expand.grid(height = seq(60, 80, 5), weight = seq(100, 300, 100), 
            brand = c("Belkin","Elago"))





