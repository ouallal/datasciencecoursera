install.packages("caret",dependencies = T)
library(caret)
library(kernlab)
data(spam)
intrain <- createDataPartition(y=spam$type,p = 0.75,list = F)
traing <- spam[intrain,]
testing <- spam[- intrain,]
set.seed(32343)
modelfit <- train(type~.,data = traing,method ="glm")
modelfit$finalModel
prediction <- predict(modelfit,newdata= testing)
length(prediction)
confusionMatrix(prediction,testing$type)
args(train.formula)
install.packages("ISLR")
library(ISLR)
library(ggplot2)
data(Wage)
summary(Wage)
intrain2 <- createDataPartition(y= Wage$wage,p = 0.7,list = F)
traing2 <- Wage[intrain2,]
testing2<- Wage[-intrain2,]
featurePlot(x=traing2[,c("age","education","jobclass")],y = traing2$wage,plot = "pairs")
qplot(x = age,y = wage,data = traing2,col= education)+geom_smooth(method = "lm",formula = y~x)
install.packages("Hmisc")
library(Hmisc)
wagecat <- cut2(traing2$wage,g=5)
t1 <- table(wagecat,traing2$jobclass)
prop.table(t1,2)
qplot(wage,col=education,data = traing2,geom = "density")

## preprocessing 
mean(traing$capitalAve)
sd(traing$capitalAve)
table(traing2$jobclass)
dammny <- dummyVars(wage~ jobclass,data = traing2)
head(predict(dammny,newdata= testing2))
zenn <- nearZeroVar(traing2,saveMetrics = T)
zenn
M <-  abs(cor(traing[,-58]))
diag(M) <- 0
which(M  > 0.8,arr.ind = T)
plot(spam[,32],spam[,34])
library(AppliedPredictiveModeling)








