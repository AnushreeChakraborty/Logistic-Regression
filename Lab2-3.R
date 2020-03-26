df<-read.csv("D:/R/data sets/seeds_dataset.csv")
head(df)
dim(df)
colnames(df)
levels(df$class)
View(df$class)
df$class<-factor(df$class)
str(df)
any(is.na(df))

library(ggplot2)
ggplot(df,aes(perimeter)) + geom_histogram()
ggplot(df,aes(area)) + geom_histogram()

library(caTools)
sample<-sample.split(df$class, SplitRatio = 0.70)
train<-subset(df, sample==TRUE)
test<-subset(df, sample==FALSE)

#set 1 as the baseline
library(nnet)
train$class<-relevel(train$class,ref = '1')

multinom.fit<-multinom(class ~ .-1, data = train)
summary(multinom.fit)

head(probability.table<-fitted(multinom.fit))
train$predicted<-predict(multinom.fit, newdata=train, 'class')

ctable<-table(train$class, train$predicted)
ctable

round((sum(diag(ctable))/sum(ctable))*100,2)

test$predicted<-predict(multinom.fit, newdata=test, 'class')
ctable2<-table(test$class, test$predicted)
ctable2

round((sum(diag(ctable2))/sum(ctable2))*100,2)

library(pROC)
library(nnet)
mn.net <- nnet::multinom(class ~ ., train)
test_prob=predict(mn.net, newdata=test, type='prob')
test_roc=multiclass.roc(test$class ~ test_prob, plot=TRUE, print.AUC=TRUE)

