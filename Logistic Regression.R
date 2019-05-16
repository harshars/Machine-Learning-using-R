insurance = read.csv("D:/regression/insurance.csv", header = TRUE, sep = ",")

summary(insurance)

library(caTools)

#Splitting the data
split <- sample.split(insurance, SplitRatio = 0.7)

set.seed(88)

train <- subset(insurance, split == TRUE)
test <- subset(insurance, split == FALSE)


response_rate = (sum(train$target)/length(train$target))*100


#Response rate is 7%, hence we need not over sample the training data

# Transformation Code

for(i in colnames(train1)) {
  temp = train[,i]
  max = cor(temp,train[,14])
  temp = log(temp)
  if(cor(temp,train[,14]) > max) {
    train1[,i]=temp
    max = cor(temp,train[,14])
    print(paste(i,"-","log"))
  }
  temp = 1/train[,i]
  if(cor(temp,train[,14]) > max) {
    train1[,i]=temp
    max = cor(temp,train[,14])
    print(paste(i,"-","inverse"))
  }
  temp = train[,i]^2
  if(cor(temp,train[,14]) > max) {
    train1[,i]= temp
    max = cor(temp,train[,14])
    print(paste(i,"-","square"))
  }
  temp = train[,i]^(1/2)
  if(cor(temp,train[,14]) > max) {
    train1[,i]= temp
    max = cor(temp,train[,14])
    print(paste(i,"-","square root"))
  }
  temp = train[,i]^(1/3)
  if(cor(temp,train[,14]) > max) {
    train1[,i]= temp
    max = cor(temp,train[,14])
    print(paste(i,"-","cube root"))
  }
  
}

#install.packages("ROSE")
library("ROSE")

#under_sampled_train = as.data.frame(ovun.sample(target ~ .,data=train,method="under")$data)


model = glm(target ~ .,data=train,family = binomial)
summary(model)

# Getting WOE and IV

# install.packages("InformationValue")
library(InformationValue)

# install.packages("woe")
library(woe)

#install.packages("Information")
library(Information)
## WoE & IV

iv_temp = create_infotables(data=train,y="target",bins=10)
iv = iv_temp$Summary
iv

iv_temp$Tables

#---------------------------- week 2 ----------------------------

cor(insurance)
library(car)
vif(model)

# ----------------------------- week 3 --------------------------------

predict = predict(model,newdata=train,type="response")

finaldata = cbind(train,predict)

#concordance
Concordance(actuals = finaldata$target,predictedScores =finaldata$predict)

#Gain/ Lift chart
lift <- function(depvar, predcol, groups=10) {
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)}
  if(is.factor(depvar)) depvar <- as.integer(as.character(depvar))
  if(is.factor(predcol)) predcol <- as.integer(as.character(predcol))
  helper = data.frame(cbind(depvar, predcol))
  helper[,"bucket"] = ntile(-helper[,"predcol"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(depvar), funs(total = n(),
                                    totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

dt = as.data.frame(lift(finaldata$target,finaldata$predict, groups = 10))

plot(dt$bucket, dt$Cumlift, type="l", ylab="Cumulative lift", xlab="Bucket")

#Get the gain chart using ks_stat 

#KS chart
library(InformationValue)
ks_plot(actuals=finaldata$target,predictedScores=finaldata$predict)
ks_stat(actuals=train$target,predictedScores=predict,returnKSTable = TRUE)


# ROC and AUC


pred_val <- prediction(predictions = finaldata$predict,finaldata$target)
attr(performance(pred_val,"auc"),"y.values")


ROCRperf <- performance(pred_val, 'tpr','fpr')
plot(ROCRperf)

# Precision and Recall to get cut off value

perfspec <- performance(prediction.obj = pred_val, measure="rec", x.measure="cutoff")

plot(perfspec)

par(new=TRUE)

perfsens <- performance(prediction.obj = pred_val, measure="prec", x.measure="cutoff")

plot(perfsens)

# Get 1 or 0 based on threshold value

finaldata$predict1_0 = ifelse(finaldata$predict >=0.14,1,0)


#Confusion matrix

conf <- table(finaldata$target,finaldata$predict1_0)

library(caret) 
f.conf <- confusionMatrix(conf,positive = "1")
print(f.conf)


#------------------------------------Week 4 -----------------------------------

#Predict for test data and various evaluation metrics

predict = predict(model,newdata=test,type="response")

finaldata = cbind(test,predict)

#Concordance

Concordance(actuals = finaldata$target,predictedScores =finaldata$predict)

#Gain//Lift charts

lift <- function(depvar, predcol, groups=10) {
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)}
  if(is.factor(depvar)) depvar <- as.integer(as.character(depvar))
  if(is.factor(predcol)) predcol <- as.integer(as.character(predcol))
  helper = data.frame(cbind(depvar, predcol))
  helper[,"bucket"] = ntile(-helper[,"predcol"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(depvar), funs(total = n(),
                                    totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

dt = as.data.frame(lift(finaldata$target,finaldata$predict, groups = 10))

plot(dt$bucket, dt$Cumlift, type="l", ylab="Cumulative lift", xlab="Bucket")


#KS Plot 

library(InformationValue)
ks_plot(actuals=finaldata$target,predictedScores=finaldata$predict)
ks_stat(actuals=finaldata$target,predictedScores=predict,returnKSTable = TRUE)

#ROC and AUC

pred_val <- prediction(predictions = finaldata$predict,finaldata$target)
attr(performance(pred_val,"auc"),"y.values")


ROCRperf <- performance(pred_val, 'tpr','fpr')
plot(ROCRperf)


# Get 1 or 0 based on threshold value

finaldata$predict1_0 = ifelse(finaldata$predict >=0.14,1,0)

#Confusion matrix


conf <- table(finaldata$target,finaldata$predict1_0)

library(caret) 
f.conf <- confusionMatrix(conf,positive = "1")
print(f.conf)







train = within(train,rm("tage5"))
model = glm(train$target ~ .,
            data=train[,!(colnames(train) %in% c())],
            family = "binomial")
summary(model)
predict = predict.glm(model,newdata=train,type="response")
Concordance(actuals = train$target,predictedScores =predict)
vif(model)




null = glm(as.factor(target) ~ 1,data=train, family="binomial")
full = glm(as.factor(target) ~ .,data=train,family="binomial")
step(null, scope=list(lower=null, upper=full), direction="forward")





model = glm(test$target ~ .,
            data=test[,!(colnames(test) %in% c())],
            family = "binomial")
summary(model)
predict = predict.glm(model,newdata=test,type="response")

Concordance(actuals = test$target,predictedScores =predict)
vif(model)




