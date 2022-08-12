
if(!require('corrplot')){
  install.packages('corrplot')
  library(corrplot)
}

if(!require('gridExtra')){
  install.packages('gridExtra')
  library(gridExtra)
}

if(!require('caret')){
  install.packages('caret')
  library(caret)
}

if(!require('MASS')){
  install.packages('MASS')
  library(MASS)
}

if(!require('partykit')){
  install.packages('partykit')
  library(partykit)
}


if(!require('h2o')){
  install.packages('h2o')
  library(h2o)
}

if(!require('data.table')){
  install.packages('data.table')
  library(data.table)
}

if(!require('randomForest')){
  install.packages('randomForest')
  library(randomForest)
}

if(!require('tidyverse')){
  install.packages('tidyverse')
  library(tidyverse)
}

if(!require('Metrics')){
  install.packages('Metrics')
  library(Metrics)
}

if(!require('caTools')){
  install.packages('caTools')
  library(caTools)
}

if(!require('smotefamily')){
  install.packages('smotefamily')
  library(smotefamily)
}

if(!require('imbalance')){
  install.packages('imbalance')
  library(imbalance)
}

if(!require('ROSE')){
  install.packages('ROSE')
  library(ROSE)
}

if(!require('rpart')){
  install.packages('rpart')
  library(rpart)
}

if(!require('pROC')){
  install.packages('pROC')
  library(pROC)
}

if(!require('regclass')){
  install.packages('regclass')
  library(regclass)
}

if(!require('DMwR')){
  remotes::install_github("cran/DMwR")
  library(DMwR)
}

if(!require('gbm')){
  install.packages('gbm')
  library(gbm)
}

set.seed(12345)

### Preparation for training ###


df12<- merge(df_2_cli_account_clean, df_1_cli_fid_clean, by = c("ID_CLI", "ID_CLI"))
View(df12)
data<- df12[,c(1,2,3,4,5,6,9,10,12)]
data$TYP_CLI_ACCOUNT<-as.factor(data$TYP_CLI_ACCOUNT)
data<-setDT(data)[,c(levels(data$TYP_CLI_ACCOUNT), "TYP_CLI_ACCOUNT") := c(lapply(levels(TYP_CLI_ACCOUNT),function(x) as.integer(x == TYP_CLI_ACCOUNT)), .(NULL))]
colnames(data)[9] <- "TYP_CLI_2"
colnames(data)[10] <- "TYP_CLI_4"
View(data)


unione<- merge(df_6_camp_event_clean_final, data, by = c("ID_CLI", "ID_CLI"))
View(unione)
data1<-unione[,c(1,2,3,5,16,19,20,21,22,23)]


data1<-unique(data1)
data1$LAST_STATUS_FID<-factor(data1$LAST_STATUS_FID)
table(data1$LAST_STATUS_FID)
data2<-SMOTE(LAST_STATUS_FID~., data1, perc.over = 180, perc.under = 200 )
table(data2$LAST_STATUS_FID)

data<-data2

intrain<- createDataPartition(data$LAST_STATUS_FID, p = .5,list=FALSE)
train<-data[intrain,]
test<-data[-intrain,]

######REGRESSIONE LOGISTICA#######################
model<-glm(LAST_STATUS_FID~LAST_TYP_CLI_FID+ID_CAMP+FIRST_ID_NEG+ID_ADDRESS, family='binomial', train) 

summary(model)

prediction<-predict(model, test, type='response')

model.AUC<-colAUC(prediction, test$LAST_STATUS_FID, plotROC = T)
abline(h = model.AUC, col='green')
text(.2,.9, cex=.8, labels = paste("Optimal cutoff:",round(model.AUC,4)))

cut = paste(round(model.AUC,4))
dat.Class<- ifelse(prediction > cut, 1,0)

dat.Class<-as.factor(dat.Class)
test$LAST_STATUS_FID <-as.factor(test$LAST_STATUS_FID)

cf<-confusionMatrix(dat.Class, test$LAST_STATUS_FID)
cf

print(accuracy(test$LAST_STATUS_FID, dat.Class))
recall(test$LAST_STATUS_FID, prediction)

###########RANDOM FOREST##############################

fit_rf <- randomForest(LAST_STATUS_FID~LAST_TYP_CLI_FID+ID_CAMP+FIRST_ID_NEG+ID_ADDRESS, data=train, proximity=FALSE,importance = FALSE, ntree=500)
fit_rf

predrf <- predict(fit_rf, data = test, type = "response")
View(predrf)
test1<-test
test1$LAST_STATUS_FID_PRED<-predrf
View(test1)
rftab <- table(predrf, test$LAST_STATUS_FID)
rftab
predrf<- as.factor(predrf)
test$LAST_STATUS_FID<-as.factor(test$LAST_STATUS_FID)
print(accuracy(predrf, test$LAST_STATUS_FID))
recall <- rftab[1,1]/(rftab[1,1]+rftab[2,1])
print(recall)
precision<- rftab[1,1]/(rftab[1,1]+rftab[1,2])
print(precision)

##########ALBERO DI DECISIONE###################

rpart <- rpart(LAST_STATUS_FID~LAST_TYP_CLI_FID+ID_CAMP+FIRST_ID_NEG+ID_ADDRESS, data = train, method = "class", control = rpart.control((cp = 0.05)))
summary(rpart)

rpred <- predict(rpart, data = test, type = "class")
dtab1 <- table(rpred, test$LAST_STATUS_FID)
dtab1

print(accuracy(test$LAST_STATUS_FID, rpred))
recall <- dtab1[1,1]/(dtab1[1,1]+dtab1[2,1])
print(recall)
precision<- dtab1[1,1]/(dtab1[1,1]+dtab1[1,2])
print(precision)

#########CURVE ROC###############################

glm.roc <- roc(response = test$LAST_STATUS_FID, predictor = as.numeric(prediction))
rpart.roc <- roc(response = train$LAST_STATUS_FID, predictor = as.numeric(rpred))
rf.roc <- roc(response = test$LAST_STATUS_FID, predictor = as.numeric(predrf))


plot(glm.roc, col = "black",legacy.axes = TRUE, print.auc.y = 0.85, print.auc.x = 1, print.auc = TRUE)
plot(rpart.roc, col = "blue", add = TRUE, print.auc.y = 0.65, print.auc.x = 1,print.auc = TRUE)
plot(rf.roc, col = "red" , add = TRUE, print.auc.y = 1, print.auc.x = 1,print.auc = TRUE)

legend("bottom", c("Random Forest", "Decision Tree", "Logistic"),
       lty = c(1,1), lwd = c(2, 2), col = c("red", "blue", "black"), cex = 0.75)

churn_pred <- test1[,c(1,11)]
 

