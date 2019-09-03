library(caret)
library(pROC)
library(mlbench)
data<-read.csv('cancer.csv')
head(data,2)
str(data)
data$Outcome[data$Outcome==0]<-'NO'
data$Outcome[data$Outcome==1]<-'YES'
data$Outcome<-factor(data$Outcome)
str(data)


set.seed(1234)

ind<-sample(2,nrow(data),replace=T,prob=c(0.7,0.3))
training<-data[ind==1,]
test<-data[ind==2,]


#Knn

trControl<-trainControl(method='repeatedcv',number=10,repeats=3)
fit<-train(Outcome ~ .,data=training,method='knn',trControl=trControl,
           preProc=c('center','scale'))
fit
plot(fit)
varImp(fit)
pred<-predict(fit,newdata=test)
pred
class(test)
head(data)
x=data.frame('AGE'=59,'SMOKING'=1,'YELLOW_FINGERS'=1,'ANXIETY'=1,'PEER_PRESSURE'=2,
             'CHRONIC.DISEASE'=1,'FATIGUE'=2,'ALLERGY'=1,'WHEEZING'=2,
             'ALCOHOL.CONSUMING'=1, 'COUGHING'=2,'SHORTNESS.OF.BREATH'=2  ,
             'SWALLOWING.DIFFICULTY'=1,'CHEST.PAIN'=2)
predict(fit,x)


######LOGISTIC REGRESSION

logitmod<-glm(Outcome ~ .,family = 'binomial',data=training)
summary(logitmod)
predict<-predict(logitmod,newdata=test,type='response')
predict
y_pred_num<-ifelse(predict>0.5,'YES','NO')
y_predict<-factor(y_pred_num)
y_predict
y_act<-test$Outcome
y_act
x=data.frame('AGE'=59,'SMOKING'=1,'YELLOW_FINGERS'=1,'ANXIETY'=1,'PEER_PRESSURE'=2,
             'CHRONIC.DISEASE'=1,'FATIGUE'=2,'ALLERGY'=1,'WHEEZING'=2,
             'ALCOHOL.CONSUMING'=1, 'COUGHING'=2,'SHORTNESS.OF.BREATH'=2  ,
             'SWALLOWING.DIFFICULTY'=1,'CHEST.PAIN'=2)
testpredict<-predict(logitmod,x)
y_predict1<-ifelse(testpredict>0.5,'YES','NO')
y_pred<-factor(y_predict1)
y_pred

#######SVM

library("e1071")
svm_model <- svm(Outcome ~ ., data=training)
summary(svm_model)
y_predicted<-predict(svm_model,newdata=test)
y_predicted
x=data.frame('AGE'=59,'SMOKING'=1,'YELLOW_FINGERS'=1,'ANXIETY'=1,'PEER_PRESSURE'=2,
             'CHRONIC.DISEASE'=1,'FATIGUE'=2,'ALLERGY'=1,'WHEEZING'=2,
             'ALCOHOL.CONSUMING'=1, 'COUGHING'=2,'SHORTNESS.OF.BREATH'=2  ,
             'SWALLOWING.DIFFICULTY'=1,'CHEST.PAIN'=2)
y_predict<-predict(svm_model,x)
y_predict
