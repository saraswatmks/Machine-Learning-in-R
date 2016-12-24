#set working directory
path <- "~/December 2016/XGBoost_Tutorial"
setwd(path)

#load libraries
library(data.table)
library(mlr)

#set variable names
setcol <- c("age",
            "workclass",
            "fnlwgt",
            "education",
            "education-num",
            "marital-status",
            "occupation",
            "relationship",
            "race",
            "sex",
            "capital-gain",
            "capital-loss",
            "hours-per-week",
            "native-country",
            "target")

#load data
train <- read.table("adultdata.txt",header = F,sep = ",",col.names = setcol,na.strings = c(" ?"),stringsAsFactors = F)
test <- read.table("adulttest.txt",header = F,sep = ",",col.names = setcol,skip = 1, na.strings = c(" ?"),stringsAsFactors = F)

#convert data frame to data table
setDT(train)
setDT(test)

#check missing values
table(is.na(train))
sapply(train, function(x) sum(is.na(x))/length(x))*100
table(is.na(test))
sapply(test, function(x) sum(is.na(x))/length(x))*100

#quick data cleaning
#remove extra character from target variable
library(stringr)
test[,target := substr(target,start = 1,stop = nchar(target)-1)]

#remove leading whitespaces
char_col <- colnames(train)[sapply(test,is.character)]
for(i in char_col)
      set(train,j=i,value = str_trim(train[[i]],side = "left"))
for(i in char_col)
      set(test,j=i,value = str_trim(test[[i]],side = "left"))

#set all missing value as "Missing"
train[is.na(train)] <- "Missing"
test[is.na(test)] <- "Missing"

#using one hot encoding
labels <- train$target
ts_label <- test$target
new_tr <- model.matrix(~.+0,data = train[,-c("target"),with=F])
new_ts <- model.matrix(~.+0,data = test[,-c("target"),with=F])

#convert factor to numeric
labels <- as.numeric(labels)-1
ts_label <- as.numeric(ts_label)-1

#preparing matrix
dtrain <- xgb.DMatrix(data = new_tr,label = labels)
dtest <- xgb.DMatrix(data = new_ts,label=ts_label)

#default parameters
params <- list(
        booster = "gbtree",
        objective = "binary:logistic",
        eta=0.3,
        gamma=0,
        max_depth=6,
        min_child_weight=1,
        subsample=1,
        colsample_bytree=1
)

xgbcv <- xgb.cv(params = params
                ,data = dtrain
                ,nrounds = 100
                ,nfold = 5
                ,showsd = T
                ,stratified = T
                ,print.every.n = 10
                ,early.stop.round = 20
                ,maximize = F
)
##best iteration = 79

min(xgbcv$test.error.mean)
#0.1263

#first default - model training
xgb1 <- xgb.train(
           params = params
          ,data = dtrain
          ,nrounds = 79
          ,watchlist = list(val=dtest,train=dtrain)
          ,print.every.n = 10
          ,early.stop.round = 10
          ,maximize = F
          ,eval_metric = "error"
)

#model prediction
xgbpred <- predict(xgb1,dtest)
xgbpred <- ifelse(xgbpred > 0.5,1,0)

#confusion matrix
library(caret)
confusionMatrix(xgbpred, ts_label)
#Accuracy - 86.54%

#view variable importance plot
mat <- xgb.importance(feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance(importance_matrix = mat[1:20]) #first 20 variables

#convert characters to factors
fact_col <- colnames(train)[sapply(train,is.character)]
for(i in fact_col)
        set(train,j=i,value = factor(train[[i]]))
for(i in fact_col)
        set(test,j=i,value = factor(test[[i]]))

#create tasks
traintask <- makeClassifTask(data = train,target = "target")
testtask <- makeClassifTask(data = test,target = "target")

#do one hot encoding
traintask <- createDummyFeatures(obj = traintask,target = "target")
testtask <- createDummyFeatures(obj = testtask,target = "target")

#create learner
lrn <- makeLearner("classif.xgboost",predict.type = "response")
lrn$par.vals <- list(
             objective="binary:logistic",
             eval_metric="error",
             nrounds=100L,
             eta=0.1
)

#set parameter space
params <- makeParamSet(
         makeDiscreteParam("booster",values = c("gbtree","gblinear")),
         makeIntegerParam("max_depth",lower = 3L,upper = 10L),
         makeNumericParam("min_child_weight",lower = 1L,upper = 10L),
         makeNumericParam("subsample",lower = 0.5,upper = 1),
         makeNumericParam("colsample_bytree",lower = 0.5,upper = 1)
)

#set resampling strategy
rdesc <- makeResampleDesc("CV",stratify = T,iters=5L)

#search strategy
ctrl <- makeTuneControlRandom(maxit = 10L)

#set parallel backend
library(parallel)
library(parallelMap)
parallelStartSocket(cpus = detectCores())

#parameter tuning
mytune <- tuneParams(learner = lrn
               ,task = traintask
               ,resampling = rdesc
               ,measures = acc
               ,par.set = params
               ,control = ctrl
               ,show.info = T)

mytune$y #0.873069

#set hyperparameters
lrn_tune <- setHyperPars(lrn,par.vals = mytune$x)

#train model
xgmodel <- train(learner = lrn_tune,task = traintask)

#predict model
xgpred <- predict(xgmodel,testtask)

confusionMatrix(xgpred$data$response,xgpred$data$truth)
#Accuracy : 0.8747

#stop parallelization
parallelStop()
