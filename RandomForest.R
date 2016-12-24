path <- "~/December 2016/RF_Tutorial"
setwd(path)

#load libraries and data
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

setDT(train)
setDT(test)

#Data Sanity
dim(train) #32561 X 15
dim(test) #16281 X 15
str(train)
str(test)

#check missing values
table(is.na(train))
sapply(train, function(x) sum(is.na(x))/length(x))*100

table(is.na(test))
sapply(test, function(x) sum(is.na(x))/length(x))*100


#check target variable
#binary in nature check if data is imbalanced
train[,.N/nrow(train),target]
test[,.N/nrow(test),target]
test[,target := substr(target,start = 1,stop = nchar(target)-1)]

#remove leading whitespace
library(stringr)
char_col <- colnames(train)[sapply(test,is.character)]
for(i in char_col)
        set(train,j=i,value = str_trim(train[[i]],side = "left"))


#set all character variables as factor
fact_col <- colnames(train)[sapply(train,is.character)]

for(i in fact_col)
        set(train,j=i,value = factor(train[[i]]))

for(i in fact_col)
        set(test,j=i,value = factor(test[[i]]))


#impute missing values
imp1 <- impute(data = train,target = "target",classes = list(integer=imputeMedian(), factor=imputeMode()))
imp2 <- impute(data = test,target = "target",classes = list(integer=imputeMedian(), factor=imputeMode()))
train <- setDT(imp1$data)
test <- setDT(imp2$data)
            
            
#use MLR
#create a task
traintask <- makeClassifTask(data = train,target = "target")
testtask <- makeClassifTask(data = test,target = "target")

#create learner
bag <- makeLearner("classif.rpart",predict.type = "response")
bag.lrn <- makeBaggingWrapper(learner = bag,bw.iters = 100,bw.replace = TRUE)

rdesc <- makeResampleDesc("CV",iters=5L)

#set parallel backend
library(parallelMap)
library(parallel)
parallelStartSocket(cpus = detectCores())
#parallelstart

#Bagging
system.time(
r <- resample(learner = bag.lrn
              ,task = traintask
              ,resampling = rdesc
              ,measures = list(tpr,fpr,fnr,fpr,acc)
              ,show.info = T)
)

#[Resample] Result: 
# tpr.test.mean=0.95,
# fpr.test.mean=0.487,
# fnr.test.mean=0.0505,
# acc.test.mean=0.845


#Random Forest without Cutoff
rf.lrn <- makeLearner("classif.randomForest")
rf.lrn$par.vals <- list(ntree = 100L,
                        importance=TRUE)

r <- resample(learner = rf.lrn
              ,task = traintask
              ,resampling = rdesc
              ,measures = list(tpr,fpr,fnr,fpr,acc)
              ,show.info = T)

# Result:
# tpr.test.mean=0.996,
# fpr.test.mean=0.72,
# fnr.test.mean=0.0034,
# acc.test.mean=0.825


#Random Forest with cutoff
rf.lrn <- makeLearner("classif.randomForest")
rf.lrn$par.vals <- list(ntree = 100L,
                        importance=TRUE,
                        cutoff = c(0.75,0.25))

r <- resample(learner = rf.lrn
              ,task = traintask
              ,resampling = rdesc
              ,measures = list(tpr,fpr,fnr,fpr,acc)
              ,show.info = T)

#Result: tpr.test.mean=0.934,
# fpr.test.mean=0.43,
# fnr.test.mean=0.0662,
# acc.test.mean=0.846


#Random Forest Tuning
getParamSet(rf.lrn)
rf.lrn$par.vals <- list(ntree = 100L,
                        importance=TRUE,
                        cutoff=c(0.75,0.25)
)

#set parameter space
params <- makeParamSet(
        makeIntegerParam("mtry",lower = 2,upper = 10),
        makeIntegerParam("nodesize",lower = 10,upper = 50)
)

#set validation strategy
rdesc <- makeResampleDesc("CV",iters=5L)

#set optimization technique
ctrl <- makeTuneControlRandom(maxit = 5L)

library(parallelMap)
library(parallel)
parallelStartSocket(cpus = detectCores())

tune <- tuneParams(learner = rf.lrn
                   ,task = traintask
                   ,resampling = rdesc
                   ,measures = list(acc)
                   ,par.set = params
                   ,control = ctrl
                   ,show.info = T)
[Tune] Result: mtry=2; nodesize=23 : acc.test.mean=0.858

#stop parallelization
parallelStop()
