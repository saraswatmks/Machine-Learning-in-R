path <- "~/Data/Imbalanced"
setwd(path)

#load data
library(data.table)
train <- fread("train.csv",na.strings = c(""," ","?",NA,"NA"))
test <- fread("test.csv",na.strings = c(""," ","?",NA,"NA"))

#check data
str(train)
str(test)

#encode target variables
train[,income_level := ifelse(income_level == "-50000",0,1)]
test[,income_level := ifelse(income_level == "-50000",0,1)]


#set column classes
#ft <- names(train)[which(sapply(train, is.numeric))]
#for(i in ft) set(train, j=i,value=as.numeric(train[[i]]))

factcols <- c(2:5,7,8:16,20:29,31:38,40,41)
numcols <- setdiff(1:40,factcols)

train[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
train[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

test[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
test[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

#subset categorical variables
cat_train <- train[,factcols, with=FALSE]
cat_test <- test[,factcols,with=FALSE]

#subset numerical variables
num_train <- train[,numcols,with=FALSE]
num_test <- test[,numcols,with=FALSE]

rm(train,test) 
gc() #garbage collection saves memory


#work on numerical data
#check correlation & remove correlated variable
library(caret)

ax <-findCorrelation(x = cor(num_train), cutoff = 0.7)

num_train <- num_train[,-ax,with=FALSE] #removed weeks_worked_in_year
num_test[,weeks_worked_in_year := NULL] 

#add dependent variables for data visualization
num_train[,income_level := cat_train$income_level]

#load libraries
library(ggplot2)
library(plotly)

#write a function
tr <- function(a){
        ggplot(data = num_train, aes(x= a, y=..density..))+geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) + geom_density()
        ggplotly()
}

tr(num_train$age)
tr(num_train$capital_losses)

#create scatter plot by target variable
ggplot(data=num_train,aes(x = age, y=wage_per_hour))+geom_point(aes(colour=income_level))+scale_y_continuous("wage per hour", breaks = seq(0,10000,1000))
ggplot(data=num_train,aes(x = age, y=capital_gains))+geom_point(aes(colour=income_level))


#check missing values in numerical data - none available
table(is.na(num_train))
table(is.na(num_test))


#work on categorical data
all_bar <- function(i){
        ggplot(cat_train,aes(x=i,fill=income_level))+geom_bar(position = "dodge", color="black")+scale_fill_brewer(palette = "Pastel1")+theme(axis.text.x =element_text(angle = 60,hjust = 1,size = 10))
}

all_bar(cat_train$class_of_worker)
all_bar(cat_train$education)


#combine factor levels with less than 5% values
#train
for(i in names(cat_train)){
        p <- 5/100
        ld <- names(which(prop.table(table(cat_train[[i]])) < p))
        levels(cat_train[[i]])[levels(cat_train[[i]]) %in% ld] <- "Other"
}


#test
for(i in names(cat_test)){
        p <- 5/100
        ld <- names(which(prop.table(table(cat_test[[i]])) < p))
        levels(cat_test[[i]])[levels(cat_test[[i]]) %in% ld] <- "Other"
}



#check missing values
table(is.na(cat_train)) #missing values found
table(is.na(cat_test))

#check missing values per columns
mvtr <- sapply(cat_train, function(x){sum(is.na(x))/length(x)})*100
mvte <- sapply(cat_test, function(x){sum(is.na(x)/length(x))}*100)
mvtr #some variables have more than 50% value
mvte

#select columns with missing value less than 5% 
cat_train <- subset(cat_train, select = mvtr < 5 ) #removed migration migration_within_reg migration_sunbelt migration_msa
cat_test <- subset(cat_test, select = mvte < 5)

#hygiene check
#check columns with unequal levels 
library(mlr)
summarizeColumns(cat_train)[,"nlevs"]
summarizeColumns(cat_test)[,"nlevs"] 

#missing value in categorical
#set NA as Unavailable - train data

#convert to characters
cat_train <- cat_train[,names(cat_train) := lapply(.SD, as.character),.SDcols = names(cat_train)]
for (i in seq_along(cat_train)) set(cat_train, i=which(is.na(cat_train[[i]])), j=i, value="Unavailable")
cat_train <- cat_train[, names(cat_train) := lapply(.SD,factor), .SDcols = names(cat_train)]

#set NA as Unavailable - test data
cat_test <- cat_test[, (names(cat_test)) := lapply(.SD, as.character), .SDcols = names(cat_test)]
for (i in seq_along(cat_test)) set(cat_test, i=which(is.na(cat_test[[i]])), j=i, value="Unavailable")
cat_test <- cat_test[, (names(cat_test)) := lapply(.SD, factor), .SDcols = names(cat_test)]

#bin age 
num_train[,.N,age][order(age)]
ggplot(num_train,aes(age,fill=income_level))+geom_bar(position = "dodge",color="black")
ggplotly()

#bin age variable 0-30 31-60 61 - 90
num_train[,age:= cut(x = age,breaks = c(0,30,60,90),include.lowest = TRUE,labels = c("young","adult","old"))]
num_train[,age := factor(age)]

num_test[,age:= cut(x = age,breaks = c(0,30,60,90),include.lowest = TRUE,labels = c("young","adult","old"))]
num_test[,age := factor(age)]

#bin wage_per_hour 0 and 1
ggplot(num_train,aes(wage_per_hour,fill=income_level))+geom_bar(position = "dodge",color="black")
ggplotly() #for interactive plot

#Bin numeric variables with Zero and MoreThanZero
num_train[,wage_per_hour := ifelse(wage_per_hour == 0,"Zero","MoreThanZero")]
num_train[,capital_gains := ifelse(capital_gains == 0,"Zero","MoreThanZero")]
num_train[,capital_losses := ifelse(capital_losses == 0,"Zero","MoreThanZero")]
num_train[,dividend_from_Stocks := ifelse(dividend_from_Stocks == 0,"Zero","MoreThanZero")]

num_train[,wage_per_hour := as.factor(wage_per_hour)]
num_train[,capital_gains := as.factor(capital_gains)]
num_train[,capital_losses := as.factor(capital_losses)]
num_train[,dividend_from_Stocks := as.factor(dividend_from_Stocks)]

num_test[,wage_per_hour := ifelse(wage_per_hour == 0,"Zero","MoreThanZero")]
num_test[,capital_gains := ifelse(capital_gains == 0,"Zero","MoreThanZero")]
num_test[,capital_losses := ifelse(capital_losses == 0,"Zero","MoreThanZero")]
num_test[,dividend_from_Stocks := ifelse(dividend_from_Stocks == 0,"Zero","MoreThanZero")]

num_test[,wage_per_hour := as.factor(wage_per_hour)]
num_test[,capital_gains := as.factor(capital_gains)]
num_test[,capital_losses := as.factor(capital_losses)]
num_test[,dividend_from_Stocks := as.factor(dividend_from_Stocks)]

num_train[,income_level := NULL]

#get ready for machine learning
#combine data
d_train <- cbind(num_train,cat_train)
d_test <- cbind(num_test,cat_test)

rm(num_train,num_test,cat_train,cat_test) #save save

#load library for machine learning
library(mlr)

#create task
train.task <- makeClassifTask(data = d_train,target = "income_level")
test.task <- makeClassifTask(data=d_test,target = "income_level") #we are considering this variable to check our accuracy

#remove zero variance features
train.task <- removeConstantFeatures(train.task)
test.task <- removeConstantFeatures(test.task)

#get variable importance chart based on information gain
var_imp <- generateFilterValuesData(train.task, method = c("information.gain"))
plotFilterValues(var_imp,feat.type.cols = TRUE)

#create balanced data sets
#undersampling #lost information
train.under <- undersample(train.task,rate = 0.1)
table(getTaskTargets(train.under))

#oversampling
train.over <- oversample(train.task,rate=15)
table(getTaskTargets(train.over))

#SMOTE
train.smote <- smote(train.task,rate = 15,nn = 5)

#somehow it did finish with memory issues
# Warning messages:
#         1: In is.factor(x) :
#         Reached total allocation of 8084Mb: see help(memory.size)
# 2: In is.factor(x) :
#         Reached total allocation of 8084Mb: see help(memory.size)
# user  system elapsed 
# 81.95   21.86  184.56 
system.time(
train.smote <- smote(train.task,rate = 10,nn = 3)
)
table(getTaskTargets(train.smote))


#build model on these 4 data sets
#lets see which algorthms should be pick
listLearners("classif","twoclass")[c("class","package")]

#naiveBayes
naive_learner <- makeLearner("classif.naiveBayes",predict.type = "response")
naive_learner$par.vals <- list(laplace = 1)

#10fold CV - stratified
folds <- makeResampleDesc("CV",iters=10,stratify = TRUE)

#cross validation results
fun_cv <- function(a){
        crv_val <- resample(naive_learner,a,folds,measures = list(acc,tpr,tnr,fpr,fp,fn))
        crv_val$aggr
        
}

fun_cv(train.task) 
# acc.test.mean tpr.test.mean tnr.test.mean fpr.test.mean 
# 0.7337249     0.8954134     0.7230270     0.2769730

fun_cv(train.under) 
# acc.test.mean tpr.test.mean tnr.test.mean fpr.test.mean 
# 0.7637315     0.9126978     0.6651696     0.3348304

fun_cv(train.over)
# acc.test.mean tpr.test.mean tnr.test.mean fpr.test.mean 
# 0.7861459     0.9145749     0.6586852     0.3413148 

fun_cv(train.smote)
# acc.test.mean tpr.test.mean tnr.test.mean fpr.test.mean 
# 0.8562135     0.9168955     0.8160638     0.1839362


#lets build our naive bayes model on train.smote
nB_model <- train(naive_learner, train.smote)
nB_predict <- predict(nB_model,test.task)
        
#evaluate
nB_prediction <- nB_predict$data$response
confusionMatrix(d_test$income_level,nB_prediction)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1
# 0 76427 17149
# 1  1067  5119
# 
# Accuracy : 0.8174         
# 95% CI : (0.815, 0.8198)
# No Information Rate : 0.7768         
# P-Value [Acc > NIR] : < 2.2e-16      
# 
# Kappa : 0.291          
# Mcnemar's Test P-Value : < 2.2e-16      
# 
# Sensitivity : 0.9862         
# Specificity : 0.2299         
# Pos Pred Value : 0.8167         
# Neg Pred Value : 0.8275         
# Prevalence : 0.7768         
# Detection Rate : 0.7661         
# Detection Prevalence : 0.9380         
# Balanced Accuracy : 0.6081         
# 
# 'Positive' Class : 0


#add class weight in algo
#use SVM
getParamSet("classif.svm")
svm_learner <- makeLearner("classif.svm",predict.type = "response")
svm_learner$par.vals<- list(class.weights = c("0"=1,"1"=10),kernel="radial")

svm_param <- makeParamSet(
        makeIntegerParam("cost",lower = 10^-1,upper = 10^2), #hard or soft margins
        makeIntegerParam("gamma",lower= 0.5,upper = 2)
)

#random search
set_search <- makeTuneControlRandom(maxit = 5L) #5 times

#cross validation #10L seem to take forever
set_cv <- makeResampleDesc("CV",iters=5L,stratify = TRUE)

#tune Params
system.time(
svm_tune <- tuneParams(learner = svm_learner,task = train.task,measures = list(acc,tpr,tnr,fpr,fp,fn), par.set = svm_param,control = set_search,resampling = set_cv)
)


#caliberated probabilities ROC decision threshold
#xgboost 
set.seed(2002)
xgb_learner <- makeLearner("classif.xgboost",predict.type = "response")

xgb_learner$par.vals <- list(
        objective = "binary:logistic",
        eval_metric = "error",
        nrounds = 150,
        print.every.n = 50
)

#define hyperparameters for tuning
xg_ps <- makeParamSet(
        makeIntegerParam("max_depth",lower=3,upper=10),
        makeNumericParam("lambda",lower=0.05,upper=0.5),
        makeNumericParam("eta", lower = 0.01, upper = 0.5),
        makeNumericParam("subsample", lower = 0.50, upper = 1),
        makeNumericParam("min_child_weight",lower=2,upper=10),
        makeNumericParam("colsample_bytree",lower = 0.50,upper = 0.80)
)

#define search function
rancontrol <- makeTuneControlRandom(maxit = 5L) #do 5 iterations

#5 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 5L,stratify = TRUE)

#tune parameters
xgb_tune <- tuneParams(learner = xgb_learner, task = train.task, resampling = set_cv, measures = list(acc,tpr,tnr,fpr,fp,fn), par.set = xg_ps, control = rancontrol)
# Tune result:
#         Op. pars: max_depth=3; lambda=0.221; eta=0.161; subsample=0.698; min_child_weight=7.67; colsample_bytree=0.642
# acc.test.mean=0.948,tpr.test.mean=0.989,tnr.test.mean=0.324,fpr.test.mean=0.676

#set optimal parameters

xgb_new <- setHyperPars(learner = xgb_learner, par.vals = xgb_tune$x)

#train model
xgmodel <- train(xgb_new, train.task)

#test model
predict.xg <- predict(xgmodel, test.task)

#make prediction
xg_prediction <- predict.xg$data$response

#make confusion matrix
xg_confused <- confusionMatrix(d_test$income_level,xg_prediction)

precision <- xg_confused$byClass['Pos Pred Value']
recall <- xg_confused$byClass['Sensitivity']

f_measure <- 2*((precision*recall)/(precision+recall))
f_measure #0.9726374 


#Xgboost AUC 
xgb_prob <- setPredictType(learner = xgb_new,predict.type = "prob")

#train model
xgmodel_prob <- train(xgb_prob,train.task)

#predict
predict.xgprob <- predict(xgmodel_prob,test.task)

#make prediction
predict.xgprob$data[1:10,]

df <- generateThreshVsPerfData(predict.xgprob,measures = list(fpr,tpr))
plotROCCurves(df)

performance(predict.xgprob,auc)
#0.9350817

#we should increase the threshold
pred2 <- setThreshold(predict.xgprob,0.4)
performance(pred2,auc) #0.9350817
confusionMatrix(d_test$income_level,pred2$data$response)
# Sensitivity : 0.9512          
# Specificity : 0.7228

pred3 <- setThreshold(predict.xgprob,0.35)
performance(pred3,auc)
confusionMatrix(d_test$income_level,pred3$data$response)
# Sensitivity : 0.9485          
# Specificity : 0.7470

pred4 <- setThreshold(predict.xgprob,0.30)
performance(pred4,auc)
confusionMatrix(d_test$income_level,pred4$data$response)
#Accuracy : 0.944 
# Sensitivity : 0.9458          
# Specificity : 0.7771

pred5 <- setThreshold(predict.xgprob,0.55)
performance(pred5,auc)
confusionMatrix(d_test$income_level,pred5$data$response)
# Accuracy : 0.9477 
# Sensitivity : 0.9596          
# Specificity : 0.6335

pred6 <- setThreshold(predict.xgprob,0.60)
performance(pred6,auc)
confusionMatrix(d_test$income_level,pred6$data$response)
#Accuracy : 0.9472
# Sensitivity : 0.9627          
# Specificity : 0.6054 

pred7 <- setThreshold(predict.xgprob,0.65)
performance(pred7,auc)
confusionMatrix(d_test$income_level,pred7$data$response)

pred8 <- setThreshold(predict.xgprob,0.70)
performance(pred8,auc)
confusionMatrix(d_test$income_level,pred8$data$response)

#check variable importance
var_imp <- generateFilterValuesData(train.task, method = c("information.gain"))
plotFilterValues(var_imp,feat.type.cols = TRUE)

#top 20 features
filtered.data <- filterFeatures(train.task,method = "information.gain",abs = 20)
filtered.data
#train
xgb_boost <- train(xgb_new,filtered.data)

#test
xgb_test <- predict(xgb_boost,test.task)
xgb_prediction <- xgb_test$data$response

#confusionMatrix
xgb_confused_again <- confusionMatrix(d_test$income_level,xgb_prediction)
xgb_confused_again

precision_xg <- xgb_confused_again$byClass['Pos Pred Value']
recall_xg <- xgb_confused_again$byClass['Sensitivity']

f_measure_xg <- 2*((precision_xg*recall_xg)/(precision_xg+recall_xg))
f_measure_xg #0.9683183

#Final Prediction Accuracy
#Accuracy : 0.944 
# Sensitivity : 0.9458 
# Specificity : 0.7771
