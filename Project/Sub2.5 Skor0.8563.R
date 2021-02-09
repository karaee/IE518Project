install.packages("tiger")
library(dummies)
library(dendextend)
library(factoextra)
library(ggpubr)
library(caret)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(spatstat)
library(dplyr)
library(prob)
library(tiger)

get_accuracy <- function(df, predicted, actual){
  confusion_table = table(predicted, df[,actual])
  TP = confusion_table[2,2]
  TN = confusion_table[1,1]
  FN = confusion_table[1,2]
  FP = confusion_table[2,1]
  accuracy = ((FP/(sum(TN+FP))+FN/(sum(FN+TP)))*0.5)
  return(accuracy)
}

setwd("C:/Users/ceren/Desktop/Dersler/2020 Fall - MASTER/IE 582/proje")

IE582_Fall20_ProjectTrain <- read_csv("IE582_Fall20_ProjectTrain.csv")
IE582_Fall20_ProjectTest <- read_csv("IE582_Fall20_ProjectTest.csv")


traindata <- IE582_Fall20_ProjectTrain
testdata <- IE582_Fall20_ProjectTest
str(traindata)
str(t2)



smpsize <- floor(0.7 * nrow(traindata))
set.seed(123)
train_ind <- sample(seq_len(nrow(traindata)), size = smpsize)
t1 <- traindata[train_ind,]
t2 <- traindata[-train_ind,]


fitControl <- trainControl(method = "repeatedcv", number = 10, verboseIter = TRUE, classProbs = TRUE)
tunegrid <- expand.grid(alpha = 1,lambda = seq(0.001,0.1,by = 0.001))
Lasso <- train(y~., data=t1, method="glmnet", trControl = fitControl, tuneGrid = tunegrid)
predLasso <- predict(Lasso,t2[,-61])
RMSEDT21 <- RMSE(as.numeric(t2$y),as.numeric(predLasso))
as.numeric(t2$y)


DTree <- rpart(y~., data = traindata, method = "class", minbucket = 10, cp = 0.01)
rpart.plot(DTree)
predDTree <- predict(DTree, testdata)


tunegrid <- expand.grid(.mtry=c(3,5,10,15), .splitrule = "gini", .min.node.size = 5)
RF <- train(y~., data=traindata, method="ranger", trControl = fitControl, tuneGrid = tunegrid)
predRF <- predict(RF, testdata)

fitControl <- trainControl(method = "repeatedcv", number = 10, verboseIter = TRUE, classProbs = TRUE)
tunegrid <-  expand.grid(interaction.depth = c(1, 5, 9),n.trees = (1:5)*50, shrinkage = c(0.05,0.01, 0.1),n.minobsinnode = 10)
SGB <- train(y~., data=traindata, method="gbm", trControl = fitControl, tuneGrid = tunegrid)
predSGB <- predict(SGB, testdata, type='prob')

SGB
predSGB$b



factor2 <- c()
for(i in 1:60)
{
  if(levels(traindata[,i])==2)
  {
    factor2 <- cbind(factor2,traindata[,i])
  }
}


trainMan <- data.frame()
for(i in 1:nrow(train3))
{
  if(train3[i,1]<50)
  {
    trainMan <- cbind.data.frame(trainMan, traindata[,i])
  }
}


to.uniform(traindata$x1)
summary(to.uniform(traindata$x14))

train3 = (t(sapply(traindata[,factor2],table)))

traindata <- as.data.frame(traindata)



factor2 <- c(factor2,36,42,50,52,61)
factor2

traindata2 <- traindata[,-factor2]
testdata2 <- testdata[,-factor2]

testdata2 <- as.data.frame(testdata2)

for(i in 1:12){
  testdata2[,i] <- to.uniform(testdata2[,i])
}

traindata3 = cbind.data.frame(traindata2,traindata[,factor2])
testdata3 = cbind.data.frame(testdata2,testdata[,factor2])


traindata4 <- traindata3[,-c(59,60)]
testdata4 <- testdata3[,-c(59,60,61)]

traindata <- as.data.frame(traindata)
factor2 <- c()
for(i in 1:60){
  if(nlevels(as.factor(traindata[,i]))==2)
  {
    factor2 <- c(factor2,i)
  }
}

factor2 <- c(factor2,36,42,50,52,61)
factor2

traindata2 <- traindata[,-factor2]

for(i in 1:12){
  traindata2[,i] <- to.uniform(traindata2[,i])
}

traindata3 = cbind.data.frame(traindata2,traindata[,factor2])

traindata4 <- traindata3[,-c(59,60)]




smpsize <- floor(0.7 * nrow(traindata4))
set.seed(123)
train_ind <- sample(seq_len(nrow(traindata4)), size = smpsize)
t1 <- traindata4[train_ind,]
t2 <- traindata4[-train_ind,]

fitControl <- trainControl(method = "repeatedcv", number = 10, verboseIter = TRUE, classProbs = TRUE)
tunegrid <- expand.grid(.mtry=c(3,5,10,15), .splitrule = "gini", .min.node.size = c(3,5,7))
RF <- train(y~., data=traindata4, method="ranger", trControl = fitControl, tuneGrid = tunegrid,num.trees=250)
RF
predRF <- predict(RF, testdata4, type='prob')
predRF$b

tunegrid <- expand.grid(alpha = 1,lambda = seq(0.001,0.1,by = 0.001))
Lasso <- train(y~., data=t1, method="glmnet", trControl = fitControl, tuneGrid = tunegrid)
predLasso <- predict(Lasso,testdata4, type='prob')
predLasso$b

rf.roc<-roc(as.numeric(t2$y),as.numeric(predRF))
auc(rf.roc)

get_accuracy <- function(predicted, actual){
  confusion_table = table(predicted,actual)
  TP = confusion_table[2,2]
  TN = confusion_table[1,1]
  FN = confusion_table[1,2]
  FP = confusion_table[2,1]
  accuracy = ((FP/(sum(TN+FP))+FN/(sum(FN+TP)))*0.5)
  return(accuracy)
}
1-get_accuracy(predRF,t2$y)

# install the required packages first
require(jsonlite)
require(httr)
require(data.table)

get_token <- function(username, password, url_site){
  
  post_body = list(username=username,password=password)
  post_url_string = paste0(url_site,'/token/')
  result = POST(post_url_string, body = post_body)
  
  # error handling (wrong credentials)
  if(result$status_code==400){
    print('Check your credentials')
    return(0)
  }
  else if (result$status_code==201){
    output = content(result)
    token = output$key
  }
  
  return(token)
}

send_submission <- function(predictions, token, url_site, submit_now=F){
  
  format_check=check_format(predictions)
  if(!format_check){
    return(FALSE)
  }
  
  post_string="list("
  for(i in 1:length(predictions)){
    if(i<length(predictions)){
      post_string=sprintf("%s%s,",post_string,predictions[i])
    } else {
      post_string=sprintf("%s%s)",post_string,predictions[i])
    }
  }
  
  submission = eval(parse(text=post_string))
  json_body = jsonlite::toJSON(submission, auto_unbox = TRUE)
  submission=list(submission=json_body)
  print(submission)
  
  if(!submit_now){
    print("You did not submit.")
    return(FALSE)      
  }
  
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  post_url_string = paste0(url_site,'/submission/')
  result = POST(post_url_string, header, body=submission)
  
  if (result$status_code==201){
    print("Successfully submitted. Below you can see the details of your submission")
  } else {
    print("Could not submit. Please check the error message below, contact the assistant if needed.")
  }
  
  print(content(result))
  
}

check_format <- function(predictions){
  
  if(all(is.numeric(predictions)) & all(predictions<=1)){
    print("Format OK")
    return(TRUE)
  } else {
    print("Wrong format")
    return(FALSE)
  }
  
}

# this part is main code
subm_url ='http://46.101.121.83'

u_name = "Los Galacticos"
p_word = "E6lOux9kirvumsWW"
submit_now = TRUE

username = u_name
password = p_word

token = get_token(username=u_name, password=p_word, url=subm_url)

predictions <- predRF$b
send_submission(predictions, token, url=subm_url, submit_now= submit_now)


# $auc
# [1] 0.9189689
# 
# $ber
# [1] 0.7937299
# 
# $score
# [1] 0.8563494
