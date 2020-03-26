library(mlbench)
data(PimaIndiansDiabetes)
dim(PimaIndiansDiabetes)
levels(PimaIndiansDiabetes$diabetes)
head(PimaIndiansDiabetes)
df<-PimaIndiansDiabetes

library(Amelia)
missmap(df, main = "Diabetes Training Data - Missing Maps", col=c('yellow','black'), legend=FALSE)   

library(ggplot2)
ggplot(df,aes(diabetes)) + geom_bar()   
ggplot(df,aes(pregnant)) + geom_bar() 
ggplot(df,aes(age)) + geom_histogram(fill='blue',bins=20,alpha=0.8)

str(df)
df$pregnant<-factor(df$pregnant)

library(caTools)
split=sample.split(df$diabetes, SplitRatio = 0.70)
final.train=subset(df, split==TRUE)
final.test=subset(df,split==FALSE)

final.log.model<-glm(formula = diabetes ~ .,family = binomial(link = 'logit'),data = final.train)
summary(final.log.model)

fitted.probabilties<-predict(final.log.model, newdata=final.test, type='response')
fitted.results<-ifelse(fitted.probabilties > 0.5, 'pos','neg')
misClasificError<-mean(fitted.results != final.test$diabetes)  
print(paste('Accuracy', 1-misClasificError))

confusion.matrix<-table(final.test$diabetes, fitted.probabilties>0.5)
TN<-confusion.matrix[1,1]
FP<-confusion.matrix[1,2]
FN<-confusion.matrix[2,1]
TP<-confusion.matrix[2,2]

print(paste("True Negative = ",TN))
print(paste("False Positive = ",FP))
print(paste("False Negative = ",FN))
print(paste("True Positive = ",TP))

Sensitivity<-TP/(TP+FN)
Sensitivity
Specificity<-TN/(TN+FP)
Specificity
Precision<-TP/(TP+FP)
Precision
False_Positive_Rate<-FP/(TN+FP)
False_Positive_Rate

library(pROC)
test_prob<-predict(final.log.model, newdata=final.test, type='response')
test_roc<-roc(final.test$diabetes ~ test_prob, plot=TRUE, print.auc=TRUE)
