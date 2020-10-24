#ESSOUNAINI SOUFIANE 

Summary:
        
        I tried to do some exploratory data analysis on the Titanic data and to predict the survivors of the Ship using neural network.


library(dplyr)
library(ggplot2)



gender<-read.csv("gender_submission.csv")
class(gender$Survived)
train<-read.csv("train.csv")
train$survived<-as.numeric(train$Survived)
sum(train$Survived)/891


Only 38.38% of people survived the crash of the Titanic.Quite tragic!
        within the document. You can embed an R code chunk like this:
        
  

test<-read.csv("test.csv")
head(train)
head(test)



train1<-train[c(-2,-13)]
datas<-rbind(train1,test)
head(datas)


## Including Plots

You can also embed plots, for example:
        
        ```{r}
data3<-train %>%
        group_by(Sex) %>%
        summarise(somme=sum(Survived),na.rm=TRUE) %>%
        select(Sex,somme)
g<-ggplot(data3,aes(x=Sex,y=somme))
g+geom_bar(stat="identity",aes(fill=Sex)) +theme_light()+labs(title = "Survivors of the titanic crash by gender",xlab="Sex",ylab="Total")


That's totally understandable.They rescued women and babies first like we have all seen in the movie.

data4<-train %>%
        group_by(Pclass) %>%
                summarise(Survivors=sum(Survived),na.rm=TRUE) %>%
        select(Pclass,Survivors)
g<-ggplot(data4,aes(x=Pclass,y=Survivors))
g+geom_bar(stat="identity",aes(fill=Pclass)) +theme_light()+labs(title = "Survivors of the titanic crash by economic class",xlab="Class",ylab="Survivors")


Like we see in the plot.People who were in the first class were more likely to survive the incident.

par(mfrow=c(1,3))
data5<-train %>%
        group_by(Pclass,Sex) %>%
                summarise(Survivors=sum(Survived),na.rm=TRUE) %>%
        select(Sex,Pclass,Survivors)
data51<-data5[1:2,]
data52<-data5[3:4,]
data53<-data5[5:6,]
head(data5)
g<-ggplot(data51,aes(x=Sex,y=Survivors))
g+geom_bar(stat="identity") +theme_light()+labs(title="Survivors by gender of the 1st economic class")
g1<-ggplot(data52,aes(x=Sex,y=Survivors))
g1+geom_bar(stat="identity") +theme_light()+labs(title="Survivors by gender of the 2nd economic class")
g2<-ggplot(data53,aes(x=Sex,y=Survivors))
g2+geom_bar(stat="identity") +theme_light()+labs(title="Survivors by gender of the 3rd economic class")

Like we see in the previous plots.Both men and women who were in the 1st class had more chance of being rescued than the others.

Looking_for_jack <- subset(train,Name=="Dawson,Mr. Jack")
head(Looking_for_jack)
```

Just for fun: 
Like you see, we couldn't find any information about Jack Dawson (The character played by Leonardo Dicaprio in the movie "Titanic")



# Data Partition
set.seed(222)
levels(train$Sex)<-c(1,0)
levels(test$Sex)<-c(1,0)
head(train)
head(test)
train$Sex<-as.numeric(train$Sex)
train$Age<-as.numeric(train$Age)
test$Sex<-as.numeric(test$Sex)
test$Age<-as.numeric(test$Age)
train$Age


# Neural Networks
library(neuralnet)
set.seed(333)
n <- neuralnet(Survived~Pclass+Sex,
               data = train,
               hidden = 5,
               err.fct = "ce",
               linear.output = FALSE)
plot(n)


# Prediction
output <- compute(n, train[,c(-2,-13)])
head(output$net.result)
head(train[c(-2,-13)])

# Confusion Matrix & Misclassification Error - training data
output <- compute(n, train[,c(-2,-13)])
p1 <- output$net.result
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(pred1, train$survived)
tab1
1-sum(diag(tab1))/sum(tab1)



We have as accracy 0.786 which is not bad at all.



here are the predictions of our survivors :
        
        

# Confusion Matrix & Misclassification Error - testing data
output <- compute(n,test)
p2 <- output$net.result
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(pred2)
tab2
1-sum(diag(tab2))/sum(tab2)


