
setwd("C:/Users/KISHOR/Desktop/DS Assign/FINANCE_PROJECT")
getwd()
library(readr)
#MFdata <- read_csv("MFdata.csv")
MFdata = read.csv("finaldata.csv")

View(MFdata)
#attach(MFdata)
str(MFdata)

MFdata_2 <- MFdata[,-c(1)]#remove the row number column
#checking the NAs in the dataset
table(is.na(MFdata_2$income))
which(is.na(MFdata_2$income) == T)

table(is.na(MFdata_2$expenses))
which(is.na(MFdata_2$expenses) == T)

table(is.na(MFdata_2$Investement.fund))
which(is.na(MFdata_2$Investement.fund) == T)

table(is.na(MFdata_2$investment.goal..in.years.))
which(is.na(MFdata_2$investment.goal..in.years.) == T)

table(is.na(MFdata_2$X1YR.returns..))
which(is.na(MFdata_2$X1YR.returns..) == T)

table(is.na(MFdata_2$X3YR.returns..))
which(is.na(MFdata_2$X3YR.returns..) == T)

table(is.na(MFdata_2$X5YR.returns..))
which(is.na(MFdata_2$X5YR.returns..) == T)

table(is.na(MFdata_2$Total.Expense..))
which(is.na(MFdata_2$Total.Expense..) == T)

str(MFdata_2)
View(MFdata_2)
MFdata_3 = MFdata_2[-c(468,449,512,527),-c(12)]#eliiminating NAs and Total expenses column
str(MFdata_3)

replaceHyphens<-function(x){
  x<-as.numeric(as.character(gsub("-", "0", x)))
}
MFdata_3$income = as.numeric(as.character(MFdata_3$income))
MFdata_3$expenses = as.numeric(as.character(MFdata_3$expenses))
MFdata_3$Investement.fund = as.numeric(as.character(MFdata_3$Investement.fund))
MFdata_3$investment.goal..in.years. = as.numeric(MFdata_3$investment.goal..in.years.)

MFdata_3$X1YR.returns.. = replaceHyphens(MFdata_3$X1YR.returns..)
MFdata_3$X3YR.returns.. = replaceHyphens(MFdata_3$X3YR.returns..)
MFdata_3$X5YR.returns.. = replaceHyphens(MFdata_3$X5YR.returns..)

final_data = MFdata_3
library(caTools)
######apply regression model(three factors for output )
set.seed(101)            #This is used to create same samples everytime
split1 = sample.split(final_data$RISK.TOLERANCE,SplitRatio = 0.75)

train = subset(final_data, split1 == TRUE)
test = subset(final_data, split1 == FALSE)
prop.table(table(train$RISK.TOLERANCE))
prop.table(table(test$RISK.TOLERANCE))

prop.table(table(final_data$RISK.TOLERANCE))

library(nnet)
library(mlogit)

model1 <- multinom(RISK.TOLERANCE ~ ., data = train, MaxNWts=5000) 
summary(model1)

pred1 <- predict(model1, train, type = "class")
pred1

t = table(pred1,train$RISK.TOLERANCE)
sum(diag(t))/sum(t) #0.881104

pred1_test <- predict(model1, test, type = "class")
pred1_test

t = table(pred1_test,test$RISK.TOLERANCE)
sum(diag(t))/sum(t)#  0.9050633
####################################################
final_data2 <- MFdata_3 

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

#normalize data
final_data2<-as.data.frame(lapply(final_data2[,-8],FUN=normalize))
final_data2 = cbind(final_data2,MFdata_3[,8])
colnames(final_data2)[24] <- "RiskTolerance"

set.seed(101)            #This is used to create same samples everytime
split2 = sample.split(final_data2$RiskTolerance,SplitRatio = 0.75)

train2 = subset(final_data2, split2 == TRUE)
test2 = subset(final_data2, split2 == FALSE)
prop.table(table(train2$RiskTolerance))
prop.table(table(test2$RiskTolerance))

prop.table(table(final_data2$RiskTolerance))

model2 <- multinom(RiskTolerance ~ ., data = train2, MaxNWts=5000) 
summary(model2)

pred2 <- predict(model2, train2, type = "class")
pred2

t = table(pred2,train2$RiskTolerance)
sum(diag(t))/sum(t) #0.881104

pred2_test <- predict(model2, test2, type = "class")
pred2_test

t = table(pred2_test,test2$RiskTolerance)
sum(diag(t))/sum(t)  #0.9050633


cor(final_data2[,-24])#correlation of 0.627(highest of all) between investfund and income 
#####################################

