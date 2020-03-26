df.train<-read.csv('D:/R/data sets/titanic.csv')
head(df.train)

#EDA
#helps in visualizing the NA values
install.packages('Amelia')   
library(Amelia)
missmap(df.train, main = "Titanic Training Data - Missing Maps", col=c('yellow','black'), legend=FALSE)   
#name of map  #first colour shows if its missing, 2nd colour, if its present 
library(ggplot2)
ggplot(df.train,aes(Survived)) + geom_bar()   #0 dead, 1 alive
ggplot(df.train,aes(Pclass)) + geom_bar(aes(fill=factor(Pclass)),alpha=0.8)
ggplot(df.train,aes(Sex)) + geom_bar(aes(fill=factor(Sex)),alpha=0.8) 
ggplot(df.train,aes(Age)) + geom_histogram(fill='blue',bins=20,alpha=0.8)  #they remved the NA values for the graph

#Data Cleaning
pl<-ggplot(df.train,aes(Pclass,Age)) + geom_boxplot(aes(group=Pclass,fill=factor(Pclass),alpha=0.5))  #since age containns a lot of missing values we try to put the mean for every class
pl+ scale_y_continuous(breaks = seq(min(0),max(80),by=2))   #showing the y values in the axis, 0,2,4,6...

impute_age<-function(age,class) 
{
  out<-age
  for(i in 1:length(age)) 
  {
    if(is.na(age[i])) 
    {
      if(class[i]==1)
        out[i]<-37
      else if (class[i]==2) 
        out[i]<-29
      else 
        out[i]<-24
    }
    else 
      out[i]<-age[i]
  }
  return(out)
}
df.train$Age<-impute_age(df.train$Age,df.train$Pclass)

#builiding the model
str(df.train)
#few parameters are not required like PassengerId,Name,Ticket,Fare,Cabin,Embarked
head(df.train,2)
library(dplyr)
df.train<-select(df.train, -PassengerId, -Name, -Ticket, -Cabin)   #we can remove some more if we want
head(df.train,2)
#convert into factor to make the data better
df.train$Survived<-factor(df.train$Survived)
df.train$Pclass<-factor(df.train$Pclass)
df.train$Parch<-factor(df.train$Parch)
df.train$SibSp<-factor(df.train$SibSp)
str(df.train)
#split
library(caTools)
set.seed(101)
split=sample.split(df.train$Survived, SplitRatio = 0.70)
final.train=subset(df.train, split==TRUE)
final.test=subset(df.train,split==FALSE)
#model
final.log.model<-glm(formula = Survived ~ .,family = binomial(link = 'logit'),data = final.train)
summary(final.log.model)
#prediction
fitted.probabilties<-predict(final.log.model, newdata=final.test, type='response')
#fixing 0 or 1
fitted.results<-ifelse(fitted.probabilties > 0.5, 1,0)
misClasificError<-mean(fitted.results != final.test$Survived)  
print(paste('Accuracy', 1-misClasificError))
#creating the confusion matrix 
table(final.test$Survived, fitted.probabilties>0.5)    #if we change that prob value the matrix will also change
#0 dead FALSE prediction that person is dead but he didnt die