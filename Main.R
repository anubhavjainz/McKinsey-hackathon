############ Importing the required libraries
library(dplyr)
library(ggplot2) #for the exploratory data analysis
library(stringr)
library(ROCR) # ROCR model validation
#Decision Tree Libraries
library(irr)
library(rpart)
library(caret)
#Tree plotting
library(rattle)
library(rpart.plot)
library(RColorBrewer)

##Random Forest Libraries
library(randomForest)


setwd("D:\\McKinsey")

getwd()

TTrain<-read.csv("train_ZoGVYWq.csv",header = T,sep = ",",na.strings = "")
TTest<-read.csv("test_66516Ee.csv",header = T,sep = ",",na.strings = "")

TTrain$Set<-"Train"
TTest$Set<-"Test"

TTest$renewal<-NA

Full<-rbind(TTrain,TTest)


########### Exploratory data analysis
str(Full)
summary(Full)


########  
#Count 3-6 6 12 has 128 missing values when premium paid =1
#Application underwriting score has 4297 missing values

#numeric vars perc_premium,age,income,Count 3-6, 6, 12,Application_underwriting_score,no_of_premiums_paid,premium
#categorical vars sourcing channel,residence area, renewal

table(Full$renewal)/79853

#0 6% 1 93%

Full$Age<-(Full$age_in_days)/365
hist(Full$Age)
hist(Full$perc_premium_paid_by_cash_credit)

hist(log(Full$Income))
summary(Full$Income)
boxplot(log(Full$Income))
#income has too much variance

hist(Full$Count_3.6_months_late)
hist(Full$Count_6.12_months_late)
hist(Full$Count_more_than_12_months_late)

hist(Full$application_underwriting_score)
#90-100
#mostly at 99

hist(Full$no_of_premiums_paid)
#mostly at 5-10

(table(Full$sourcing_channel)/114077)*100
#         A          B          C          D          E 
#54.1791948 20.5431419 15.0643863  9.4506342  0.7626428 

(table(Full$residence_area_type)/114077)*100

hist(Full$premium)


cor(Full$Income,Full$premium)
cor(log(Full$Income),Full$premium)

ggplot(data=Full,mapping = aes(y=Ratio,x=as.factor(renewal)))+geom_boxplot()



a<-Full[is.na(Full$Count_3.6_months_late),]

table(a$renewal)

b<-a%>%filter(Set=="Train")

c<-Full[!is.na(Full$Count_3.6_months_late),]

d<-c%>%filter(perc_premium_paid_by_cash_credit==1&renewal==0&no_of_premiums_paid==2)








Full$renewal<-as.factor(Full$renewal)

Full$Residence<-ifelse(Full$residence_area_type=="Urban",1,0)

Full$Channel_A<-ifelse(Full$sourcing_channel=="A",1,0)
Full$Channel_B<-ifelse(Full$sourcing_channel=="B",1,0)
Full$Channel_C<-ifelse(Full$sourcing_channel=="C",1,0)
Full$Channel_D<-ifelse(Full$sourcing_channel=="D",1,0)
Full$Channel_E<-ifelse(Full$sourcing_channel=="E",1,0)

Full$Ratio<-Full$premium/Full$Income

Full$N_Income<-(Full$Income-mean(Full$Income))/sd(Full$Income)
Full$N_Premium<-(Full$premium-mean(Full$premium))/sd(Full$premium)

#################### Spliting Train, Validation and Test DataSet
set.seed(100)
index<-sample(x = 1:nrow(Train),size = 0.8*nrow(Train),replace = F)
Train<-Full[Full$Set=="Train",]

Validate<-Train[-index,]
Train<-Train[index,]
Test<-Full[Full$Set=="Test",]

table(Train$renewal)/63882
table(Validate$renewal)/15971

#################### Applying Decision Tree Model for initial understanding of Parameters
mod<-rpart(renewal~.,data=Train[,-1],control=rpart.control(cp=0.002,maxdepth=7),method="class",parms=list(split="gini"))

mod
#Visualization of Model
fancyRpartPlot(mod)
summary(mod)
printcp(mod)
plotcp(mod, minline = TRUE)


### Model Accuracy on the Train Data Itself
actual<-Train$renewal
predicted<-predict(mod,type = "class")
actual<-as.factor(actual)

#### Kappa Metric
kappa2(data.frame(actual,predicted))

#Confusion Matric
confusionMatrix(predicted,actual,positive="1")

#ROCR curve and AUC value
head(predicted)
head(as.numeric(predicted))
predicted<-as.numeric(predicted)
predicted<-ifelse(predicted==2,1,0)

head(actual)
head(as.numeric(actual))
actual<-as.numeric(actual)
actual<-ifelse(actual==2,1,0)

pred<-prediction(actual,predicted)
perf<-performance(pred,"tpr","fpr")
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

auc<-performance(pred,"auc")
unlist(auc@y.values)

################ Testing on the Validation DataSet

actual<-Validate$renewal
predicted<-predict(mod,type = "class",newdata = Validate)
actual<-as.factor(actual)

#### Kappa Metric
kappa2(data.frame(actual,predicted))

#Confusion Matric
confusionMatrix(predicted,actual,positive="1")

#ROCR curve and AUC value
head(predicted)
head(as.numeric(predicted))
predicted<-as.numeric(predicted)
predicted<-ifelse(predicted==2,1,0)

head(actual)
head(as.numeric(actual))
actual<-as.numeric(actual)
actual<-ifelse(actual==2,1,0)

pred<-prediction(actual,predicted)
perf<-performance(pred,"tpr","fpr")
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

auc<-performance(pred,"auc")
unlist(auc@y.values)



########################################### Using Random Forest Algorithm

model1 <- randomForest(renewal~.,data=Train[,-1], importance = TRUE)
model1
a=c()
i=5
for (i in 3:6) {
  model3 <- randomForest(formula = Survived~Sex_Male+Sex_Female+Pclass_1+Pclass_2+Pclass_3+Age+SibSp+Ticket_count,ntree = 500,mtry=i, data = Train, importance = TRUE)
  predValid <- predict(model3, Validate, type = "class")
  a[i-2] = mean(predValid == Validate$Survived)
}

a

plot(3:6,a)


######max accuracy at mtry=3
model1 <- randomForest(formula = Survived~Sex_Male+Sex_Female+Pclass_1+Pclass_2+Pclass_3+Age+SibSp+Ticket_count,ntree = 500,mtry=3, data = Train, importance = TRUE)
model1


#### Model Accuracy on the Train Data Itself
actual<-Train$Survived
predicted<-predict(model1,type = "class")
actual<-as.factor(actual)

#### Kappa Metric
kappa2(data.frame(actual,predicted))

#Confusion Matric
confusionMatrix(predicted,actual,positive="1")

#ROCR curve and AUC value
head(predicted)
head(as.numeric(predicted))
predicted<-as.numeric(predicted)
predicted<-ifelse(predicted==2,1,0)

head(actual)
head(as.numeric(actual))
actual<-as.numeric(actual)
actual<-ifelse(actual==2,1,0)

pred<-prediction(actual,predicted)
perf<-performance(pred,"tpr","fpr")
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

auc<-performance(pred,"auc")
unlist(auc@y.values)

################ Testing on the Validation DataSet

actual<-Validate$Survived
predicted<-predict(model1,type = "class",newdata = Validate)
actual<-as.factor(actual)

#### Kappa Metric
kappa2(data.frame(actual,predicted))

#Confusion Matric
confusionMatrix(predicted,actual,positive="1")

#ROCR curve and AUC value
head(predicted)
head(as.numeric(predicted))
predicted<-as.numeric(predicted)
predicted<-ifelse(predicted==2,1,0)

head(actual)
head(as.numeric(actual))
actual<-as.numeric(actual)
actual<-ifelse(actual==2,1,0)

pred<-prediction(actual,predicted)
perf<-performance(pred,"tpr","fpr")
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

auc<-performance(pred,"auc")
unlist(auc@y.values)





