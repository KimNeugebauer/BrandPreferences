
View(BelkinElagoComplete)
mean(age)
mean(BelkinElagoComplete$age)
mean(BelkinElagoComplete$credit)
summary(salary)
summary(BelkinElagoComplete$salary)
plot(BelkinElagoComplete$age,BelkinElagoComplete$salary)
hist(BelkinElagoComplete$salary)
mean(BelkinElagoComplete$salary)
names(BelkinElagoComplete)<-c("belkin")
names(BelkinElagoComplete)<-c("salary","age","elevel","car","zip","credit","brand")
names(BelkinElagoComplete)<-c("salary","age","education","car","zip","credit","brand")
cor(BelkinElagoComplete$salary,BelkinElagoComplete$education)
cor(BelkinElagoComplete$salary,BelkinElagoComplete$age)
cor(BelkinElagoComplete$salary,BelkinElagoComplete$car)
cor(BelkinElagoComplete$car,BelkinElagoComplete$salary)


hist(BelkinElagoComplete$credit)
var(BelkinElagoComplete$salary)
var(BelkinElagoComplete$credit)
knitr::opts_chunk$set(echo = TRUE)


credit.regression<-glm(BelkinLagoComplete$credit~BelkinLagoComplete$salary+BelkinLagoComplete$age+BelkinLagoComplete$education)
credit.regression<-glm(BelkinElagoComplete$credit~BelkinElagoComplete$salary+BelkinElagoComplete$age+BelkinElagoComplete$education)
credit.regression<-glm(BelkinElagoComplete$credit~BelkinElagoComplete$salary+BelkinElagoComplete$age)
summary(credit.regression)
credit.regression<-glm(credit~salary+age+education,data=TRUE)
names(BelkinElagoComplete)<-c("salary","age","education","car","zip","credit","brand")
credit.regression<-glm(BelkinElagoComplete$credit~BelkinElagoComplete$salary+BelkinElagoComplete$age+BelkinElagoComplete$education)
View(credit.regression)
table(age)
table(BelkinElagoComplete$age)
table(BelkinElagoComplete$education)
BelkinElagoComplete$salary[BelkinElagoComplete$salary<100000000]
mean(BelkinElagoComplete$salary[BelkinElagoComplete$salary<100000000])
summary(BelkinElagoComplete$salary[BelkinElagoComplete$salary<100000000])
summary(BelkinElagoComplete$salary)
hist(BelkinElagoComplete$salary[BelkinElagoComplete$salary<100000000])
hist(BelkinElagoComplete$salary[BelkinElagoComplete$salary<100000000])
hist(BelkinElagoComplete$salary[BelkinElagoComplete$salary<10000000])
hist(BelkinElagoComplete$salary[BelkinElagoComplete$salary<50000000])
plot(BelkinElagoComplete$car,BelkinElagoComplete$education)
cor(BelkinElagoComplete,method=c("pearson"))
BelkinData<-c(BelkinElagoComplete)
View(BelkinData)
View(BelkinElagoComplete)
rm(BelkinData)
BelkinData<-BelkinElagoComplete
View(BelkinData)
Belkin_small<-BelkinData[BelkinData$salary<100000000]
Belkin_small<-BelkinData[BelkinData$salary<10000000]

Belkin_small<-BelkinData[BelkinData$salary<10000000,]
View(Belkin_small)

library(usethis)
