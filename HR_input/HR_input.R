setwd("C:/Users/KISHOR/Desktop/DS Assign/HR_input")
getwd()
library(readr)
HR_input <- read_csv("HR_input4_2.csv")
View(HR_input)

str(HR_input)
HR_input2 <- HR_input[,-1]

table(HR_input$Nursing_room)# count 72
which(is.na(HR_input2$Nursing_room) == T)#count =70
#since nearly half of the data is missing for Nursing_room , we decide to remove the column
final_data <- HR_input2[,-29] 

library(caTools)

set.seed(101)            #This is used to create same samples everytime
split1 = sample.split(final_data$Accept_offer,SplitRatio = 0.75)

train = subset(final_data, split1 == TRUE)
test = subset(final_data, split1 == FALSE)
prop.table(table(train$Accept_offer))
prop.table(table(test$Accept_offer))

prop.table(table(final_data$Accept_offer))
#logistic regression model
model1 <- glm(Accept_offer ~ ., data = train, family=binomial) 
summary(model1)

#prediction on train data
predict1 <- predict(model1,train,type ='response')
predict1

#confusion matrix
confusion <- table(train$Accept_offer,predict1>0.5)
confusion

#Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy#0.9528302

#ROCR curve
library(ROCR)
ROCRpred1 <- prediction(predict1,train$Accept_offer)
ROCRperf1 <- performance(ROCRpred1, 'tpr', 'fpr')
plot(ROCRperf1 , colorize=TRUE, text.adj=c(-0.2,1.7))

auc = performance(ROCRpred1, measure = "auc")
auc = auc@y.values[[1]]
auc#0.9846429

#correlation
cor(final_data)

#prediction on total data
predict2 <- predict(model1,final_data,type ='response')
predict2

#confusion matrix
confusion2 <- table(final_data$Accept_offer,predict2>0.5)
confusion2

#Accuracy 
Accuracy2 <-sum(diag(confusion2)/sum(confusion2))
Accuracy2#0.8732394

#ROCR curve
ROCRpred2 <- prediction(predict2,final_data$Accept_offer)
ROCRperf2 <- performance(ROCRpred2, 'tpr', 'fpr')
plot(ROCRperf2 , colorize=TRUE, text.adj=c(-0.2,1.7))

auc2 = performance(ROCRpred2, measure = "auc")
auc2 = auc2@y.values[[1]]
auc2#0.9044776

#Decision tree model
library(party)

dt <- ctree(train$Accept_offer ~ ., data = train)
plot(dt,main="Conditional Inference Tree")
#print(dt)
pred.response <- as.character(predict(dt),test)#predict on test data
input.response <- as.character(test$Accept_offer)
mean(input.response!=pred.response)

########
library(rpart)
library(rpart.plot)
fit<-rpart(train$Accept_offer ~ ., data = train, method ='class')
printcp(fit)
#predict on fitted data
out <- predict(fit)
pred.res <- colnames(out)[max.col(out, ties.method = c("random"))]
mean(train$Accept_offer != pred.res)#0.1981132 misclassification error

#predict on test data
out2 <- predict(fit,test)
mean(test$Accept_offer != pred.res)