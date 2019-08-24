setwd("C:/Users/derri/Onedrive/r")
diabetes <- read.csv('diabetes.csv', header = TRUE)
str(diabetes)
summary(diabetes)
library(splitstackshape)
library(ggplot2)
library(xts)
library(zoo)
library(PerformanceAnalytics)
library(lattice)
library(DAAG)
library(randomForest)
library(ResourceSelection)
library(car)
library(pROC)
library(caret)
#random sampling-------------------------------------------------------------
set.seed(1)
index <- createDataPartition(diabetes$RESPONSE, p=0.86436, list=FALSE)
Train <- diabetes[index,]
Test <- diabetes[-index,]
summary(Train)
write.csv(Train,"C:/Users/derri/Onedrive/r/train.csv",row.names = FALSE)
write.csv(Test,"C:/Users/derri/Onedrive/r/test.csv",row.names = FALSE)
Train <- read.csv('train.csv',header=TRUE)
Test <- read.csv('test.csv',header=TRUE)
#EDA------------------------------------------------------------------------------------
#correlation matrix
cormatr <- cor(Train)
symnum(cormatr)

col = colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = cormatr, col = col, symm = TRUE)
chart.Correlation(Train,histogram = TRUE,pch=19)

#Spearman Correlation Coeffients
chart.Correlation(Train,histogram = TRUE,pch=19,method = "spearman")

#ANOVA----------------------------------------------------------------------------
fit1 <- aov(PRG ~ RESPONSE, data = Train)
fit2 <- aov(PLASMA ~ RESPONSE, data = Train)
fit3 <- aov(BP ~ RESPONSE,data = Train)
fit4 <- aov(THICK ~ RESPONSE, data = Train)
fit5 <- aov(INSULIN ~ RESPONSE, data = Train)
fit6 <- aov(BODY ~ RESPONSE,data = Train)
fit7 <- aov(PEDIGREE ~ RESPONSE, data = Train)
fit8 <- aov(AGE ~ RESPONSE, data = Train)
summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)
summary(fit6)
summary(fit7)
summary(fit8)
#logistics regression
LR1 = glm(RESPONSE ~ PRG + PLASMA + BP +  THICK + INSULIN + BODY + PEDIGREE, 
                        family=binomial(link='logit'),data = Train)
summary(LR1)
logit.step1 <- step(LR1, direction = "both")
summary(logit.step1)
logit.step1a <- step(LR1, direction = "back")
logit.step1b <- step(LR1, direction = "forward")
summary(LR2)
LR2 = glm(RESPONSE ~ PRG + PLASMA  +  THICK + INSULIN + BODY + PEDIGREE, 
                     family=binomial(link='logit'),data = Train)
logit.step2 <- step(LR2, direction = "both")
summary(logit.step2)
LR3 = glm(RESPONSE ~ PRG + PLASMA + BP +  THICK + INSULIN + BODY + PEDIGREE + AGE, 
          family=binomial(link='logit'),data = Train)
logit.step3 <- step(LR3, direction = "both")
summary(logit.step3)
ht1 <- hoslem.test(Train$RESPONSE,fitted(logit.step1),g=10)
ht2 <- hoslem.test(Train$RESPONSE,fitted(logit.step2),g=10)
ht3 <- hoslem.test(Train$RESPONSE,fitted(logit.step3),g=10)
ht1
ht2
newdata <- read.csv('newdata.csv', header = TRUE)
predict(logit.step1,newdata,type="response")


OT2 = randomForest(RESPONSE~.,Train,ntree=1000,mtree=3,importance=TRUE)
print(OT)
importance(OT2)
varImpPlot(OT2)

vif(logit.step2)
vif(logit.step1)


library(reshape2) 
gg <- melt(Train) 
ggplot(gg, aes(x=value, fill=variable)) +   geom_histogram(binwidth=5) +   facet_wrap(~variable)


#decision tree------------------------------
library(rpart)
library(rpart.plot)
library(gplots)
library(ROCR)
library(rattle)
library(e1071)
Train$RESPONSE <- factor(Train$RESPONSE, levels=c(0,1), labels=c("No", "Yes"))
Test$RESPONSE <- factor(Test$RESPONSE, levels=c(0,1), labels=c("No", "Yes"))
fit=rpart(RESPONSE~.,method = "class",
          data=Train,control = rpart.control(minsplit = 1) , parms = list(split="information"))
rpart.plot(fit)
asRules(fit)

pred1 <- predict(fit, Test, type="class" )
pred1
confusionMatrix(pred1, reference = Test$RESPONSE, positive="Yes")


pred1_prob <- predict(fit, Test)
prediction1 <- prediction(pred1_prob[,2],Test$RESPONSE)
perf1 <- performance(prediction1, "tpr", "fpr")
plot(perf1, colorize=TRUE)
performance(prediction1, "auc")@y.values


fit.pru <- prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]) 
rpart.plot(fit.pru,branch=1, extra=106, under=TRUE, faclen=0,
           cex=0.8, main="¾ö²ßÊ÷")