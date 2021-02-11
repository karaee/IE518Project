IE582_Fall20_ProjectTrain <- read.csv("C:/Users/ceren/Desktop/Dersler/2020 Fall - MASTER/IE 582/proje/IE582_Fall20_ProjectTrain.csv")
IE582_Fall20_ProjectTest <- read.csv("C:/Users/ceren/Desktop/Dersler/2020 Fall - MASTER/IE 582/proje/IE582_Fall20_ProjectTest.csv")

traindata <- IE582_Fall20_ProjectTrain[,-c(50, 52, 57, 19, 26, 49, 29, 1, 47, 33, 11, 7, 2, 10, 4)]
testdata <- IE582_Fall20_ProjectTest[,-c(50, 52, 57, 19, 26, 49, 29, 1, 47, 33, 11, 7, 2, 10, 4)]

testdata <- testdata[,-length(testdata)]
View(testdata)

fitControl <- trainControl(method = "repeatedcv", number = 10, verboseIter = TRUE, classProbs = TRUE)
tunegrid2 <- expand.grid(.mtry=c(3,5,10,15), .splitrule = "gini", .min.node.size = 5)
set.seed(500)
RF10 <- train(y~., data=traindata, method="ranger", trControl = fitControl, tuneGrid = tunegrid2,num.trees = 250)
RF10
set.seed(750)
predtest10rf <- predict(RF10,testdata,type = 'prob')  #testdata 
predtest10rf$b


predictions <- predtest10rf$b

#predtest10rf$b
#
# $auc
# [1] 0.9271084
# 
# $ber
# [1] 0.7802462
# 
# $score
# [1] 0.8536773


#uptrain ve RF

set.seed(9560)

down_train <- downSample(x = traindata[, -ncol(traindata)],
                         y = as.factor(traindata$y))
table(down_train$y) 
summary(down_train)

up_train <- upSample(x = traindata[, -ncol(traindata)],
                     y = as.factor(traindata$y))                         

summary(up_train)

fitControl <- trainControl(method = "repeatedcv", number = 10, verboseIter = TRUE, classProbs = TRUE)
tunegrid2 <- expand.grid(.mtry=c(3,5,10,15), .splitrule = "gini", .min.node.size = 5)
set.seed(500)
RF11 <- train(Class~., data=up_train, method="ranger", trControl = fitControl, tuneGrid = tunegrid2)
RF11

set.seed(750)
predtest11rf <- predict(RF11,testdata,type = 'prob')  #testdata 
predtest11rf$b

#rf11
# $auc
# [1] 0.9256214
# 
# $ber
# [1] 0.8293232
# 
# $score
# [1] 0.8774723

#uptrain ve LASSO
View(traindata)

tunegrid <- expand.grid(alpha = 1,lambda = seq(0.001,0.01,by = 0.001))
Lasso <- train(Class~., data=up_train7, method="glmnet", trControl = fitControl, tuneGrid = tunegrid)
Lasso
predLasso <- predict(Lasso,traindata3)
RMSELasso <-RMSE(as.numeric(as.factor(traindata3$y)),as.numeric(predLasso))
RMSELasso



traindata <- IE582_Fall20_ProjectTrain
submitdata <- IE582_Fall20_ProjectTest
submitdata <- submitdata[,-61] # Empty y column of the submission data set


factor2 <- c()
for(i in 1:60)
{
  if(nlevels(as.factor(traindata[,i]))==2)
  {
    factor2 <- c(factor2,i)
  }
}

factor2

train3 = (t(sapply(traindata[,factor2],table)))

train3

cdeneme <- c()
for(i in 1:nrow(train3))
{
  if(train3[i,2]<=50)
  {
    cdeneme <- c(cdeneme, i)
  }
}

train3[cdeneme,]

sapply(traindata[,c(18,26,37,46,49,55,57,59,50,52)],'table')

traindata2 = traindata[,-c(18,26,37,46,49,55,57,59,50,52)]

submitdata2 = submitdata[,-c(18,26,37,46,49,55,57,59,50,52)]



sapply(traindata2,'table')

traindata2[traindata2$x42>1,'x42'] = 1

traindata2[traindata2$x36>1,'x36'] = 1

submitdata2[submitdata2$x42>1,'x42'] =1

submitdata2[submitdata2$x36>1,'x36'] =1



dim(traindata2)

factor3 <- c()
for(i in 1:50)
{
  if(nlevels(as.factor(traindata2[,i]))==2)
  {
    factor3 <- c(factor3,i)
  }
}

factor4 = c(factor3,51)

submitdata3 = submitdata2[,-factor3]

submitdata3

traindata3 = traindata2[,-factor4]

traindata3

totaldata = merge(traindata3,submitdata3,all=TRUE)

totaldata2 = scale((totaldata))

train_scaled = totaldata2[1:nrow(traindata3),]

test_scaled = totaldata2[(nrow(traindata3)+1):nrow(totaldata2),]

traindata4 = cbind.data.frame(traindata2[,factor3],train_scaled,traindata[,61])

colnames(traindata4)[51] = 'y'


testdata4 = cbind.data.frame(submitdata2[,factor3],test_scaled)


smpsize3 <- floor(0.7 * nrow(traindata4))
set.seed(123)
train_ind3 <- sample(seq_len(nrow(traindata4)), size = smpsize3)
traindata47 <- traindata4[train_ind3,]
traindata43 <- traindata4[-train_ind3,]

up_train <- upSample(x = traindata4[, -ncol(traindata4)],
                     y = as.factor(traindata4$y))                         
summary(up_train)

fitControl <- trainControl(method = "repeatedcv", number = 10, verboseIter = TRUE, classProbs = TRUE)
tunegrid2 <- expand.grid(.mtry=c(3,5,10,15), .splitrule = "gini", .min.node.size = 5)
set.seed(500)
RF12 <- train(Class~., data=up_train, method="ranger", trControl = fitControl, tuneGrid = tunegrid2)
RF12
predRF12 <- predict(RF12,testdata4, type='prob')
predRF12$b



#ilk 10 


traindata <- IE582_Fall20_ProjectTrain[,c('x30', 'x23', 'x32', 'x14', 'x56', 'x42', 'x27', 'x10', 'x9', 'x11', 'x8', 'x54', 'x5', 'x6', 'x1', 'x48', 'x7','y')]
testdata <- IE582_Fall20_ProjectTest[,c('x30', 'x23', 'x32', 'x14', 'x56', 'x42', 'x27', 'x10', 'x9', 'x11', 'x8', 'x54', 'x5', 'x6', 'x1', 'x48', 'x7')]


up_train <- upSample(x = traindata[, -ncol(traindata)],
                     y = as.factor(traindata$y))                         

summary(up_train)

fitControl <- trainControl(method = "repeatedcv", number = 10, verboseIter = TRUE, classProbs = TRUE)
tunegrid2 <- expand.grid(.mtry=c(3,5,10), .splitrule = "gini", .min.node.size = 5)
set.seed(500)
RF11 <- train(Class~., data=up_train, method="ranger", trControl = fitControl, tuneGrid = tunegrid2)
RF11

set.seed(750)
predtest11rf <- predict(RF11,testdata,type = 'prob')  #testdata 
predtest11rf$b


# $auc
# [1] 0.9084927
# 
# $ber
# [1] 0.8023446
# 
# $score
# [1] 0.8554187
