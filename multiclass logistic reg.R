#working with 3 classs 
install.packages('rattle.data')
library(rattle.data)
head(wine)
str(wine)

library(caTools)
sample<-sample.split(wine$Type, SplitRatio = 0.70)
train<-subset(wine, sample==TRUE)
test<-subset(wine, sample==FALSE)

#set 1 of the dependent variable as a baseline
install.packages('nnet')
library(nnet)
train$Type<-relevel(train$Type,ref = '3')

#here we will use multinom() instead of glm
multinom.fit<-multinom(Type ~ Alcohol+Color-1, data = train)

#we use -1 in the formula to delete the intercept
summary(multinom.fit)
#first row represents the coeff for Type 1 wine in comparison to baseline type 3, second row represents the coeff for Type 2 wine in comparison to type 3 wine

#prediction
head(probability.table<-fitted(multinom.fit))
#it indicates for the 1st obs. the probability of Type 1 is 25.6%, type 2 is 70.6%, type 3 is 3.7%

#predicting the values for train dataset
train$predicted<-predict(multinom.fit, newdata=train, 'class')

#building classification table
ctable<-table(train$Type, train$predicted)
ctable
#diagonal elements are considered

#calculating accuracy=sum of diagonal elements divided by total obs
round((sum(diag(ctable))/sum(ctable))*100,2)

#predicting the values for test dataset
test$predicted<-predict(multinom.fit, newdata=test, 'class')
ctable1<-table(test$Type, test$predicted)
ctable1
round((sum(diag(ctable1))/sum(ctable1))*100,2)
#so we have a problem of overfitting here