path <- "C:/Users/manish/Desktop/Data/Amonth_Wise/January 2016"
setwd(path)

#libraries and data
train <- read.csv("train_loan.csv",na.strings = c(""," ","NA",NA))
test <- read.csv("test_Y3wMUE5.csv",na.strings = c(""," ","NA",NA))

str(train)

#check missing values
colSums(is.na(train))
colSums(is.na(test))

prop.table(table(train$Loan_Status))

#data exploration with ggplot
library(ggplot2)
#bar with fill or stacked
ggplot(train,aes(Gender,fill=Loan_Status))+geom_bar(color="black")+scale_fill_manual(values = c("#669933","#FFcc66"))
ggplot(train,aes(Married,fill=Loan_Status))+
        geom_bar(color="black",position = "dodge")


#bar with identity stat="bin" & "identity"
ggplot(train,aes(Married,ApplicantIncome,fill=Loan_Status))+
        geom_bar(stat="identity",position = "dodge")+
        geom_text(aes(label=ApplicantIncome),color="black", vjust=0.2, check_overlap = T,position = position_dodge(.9),size=3)


#scatter plot by Target
ggplot(train,aes(ApplicantIncome,CoapplicantIncome,color=Loan_Status))+
        geom_point()
ggplot(train,aes(ApplicantIncome,CoapplicantIncome))+geom_line()+
        geom_point()

ggplot(train,aes(factor(Loan_Amount_Term),LoanAmount,fill=Loan_Status))+
        geom_bar(stat = "identity",position = "dodge")+
        guides(fill=guide_legend(reverse = TRUE)) #this is size of legend & reverse it
        
#create percentage applicant income
setDT(train)
train[,percent_appinc := (ApplicantIncome/sum(ApplicantIncome)*100)]

ggplot(train,aes(Property_Area, percent_appinc,fill=Loan_Status))+
        geom_bar(stat = "identity",position = "dodge")+
        guides(fill=guide_legend(reverse = T))

#Cleveland Dot Plot
dt <- train[1:50]
ggplot(dt,aes(ApplicantIncome,reorder(Loan_ID,ApplicantIncome)))+
        geom_point(size=2)+theme_bw()+
        theme(#panel.grid.major.x = element_blank(),
              #panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_line(colour = "grey60",linetype = "dashed"))

ggplot(dt,aes(x=ApplicantIncome,y=reorder(Loan_ID,ApplicantIncome)))+
        geom_segment(aes(yend=Loan_ID),xend=0,color="grey50")+
        geom_point(size=2,aes(color=Loan_Status))+
        scale_color_brewer(palette = "Set1",limits=c("Y","N"))+
        theme_bw()+
        theme(panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_blank(),
              legend.position=c(1,0.55),
              legend.justification=c(1,0.5))


#line plot
ggplot(train,aes(ApplicantIncome,LoanAmount,fill=Loan_Status))+
        geom_line()+
        geom_point(size=3,shape=21)+
        guides(fill=guide_legend(reverse = T))


#area plot
ggplot(train,aes(ApplicantIncome,LoanAmount,fill=Property_Area))+
        #geom_area(color="black",fill="blue",alpha=.2)+
        geom_area()

ggplot(train,aes(ApplicantIncome,LoanAmount,shape=Property_Area,colour=Property_Area))+
        #geom_area(color="black",fill="blue",alpha=.2)+
        geom_point(shape=19)+stat_smooth(method = lm)


#explore data and do necessary computation
setDT(train)
setDT(test)

train[,.N,Gender] #13NA
train[,.N,Married] #3
train[,.N,Dependents] #15NA 3+
train[,.N,Education] #not graduate 0,1
train[,.N,Self_Employed] #NA 32
ggplot(train,aes(ApplicantIncome))+geom_histogram(bins=80) #outliers + right skewed
ggplot(train,aes(CoapplicantIncome))+geom_histogram(bins=80) #outlier + right skewed
ggplot(train,aes(LoanAmount))+geom_histogram(bins=80) #outlier + right skewed
train[,.N,Loan_Amount_Term][order(-N)] #combine loan amount term
train[,.N,Credit_History] #50 NA
train[,.N,Property_Area]
train[,.N,Loan_Status]

#change factors to characters
for(i in names(train[,c(1:6,12),with=F]))
        set(train,j = i,value = as.character(train[[i]]))
for(i in names(test[,c(1:6,12),with=F]))
        set(test,j=i,value=as.character(test[[i]]))

train[,Gender := ifelse(Gender == "Male",1L,0L)]
train[is.na(Gender), Gender := -999L]
test[,Gender := ifelse(Gender == "Male",1L,0L)]
test[is.na(Gender),Gender := -999L]

train[,Married := ifelse(Married == "Yes",1L,0L)]
train[is.na(Married),Married := -999L]
test[,Married := ifelse(Married == "Yes",1L,0L)]

train[Dependents == "3+", Dependents := "3"]
train[is.na(Dependents), Dependents := "-999"]
train[,Dependents := as.integer(Dependents)]

test[Dependents == "3+", Dependents := "3"]
test[is.na(Dependents), Dependents := "-999"]
test[,Dependents := as.integer(Dependents)]

train[,Education := ifelse(Education == "Graduate",1L,0L)]
test[,Education := ifelse(Education == "Graduate",1L,0L)]

train[,Self_Employed := ifelse(Self_Employed == "No",0L,1L)]
train[is.na(Self_Employed), Self_Employed := -999L]
test[,Self_Employed := ifelse(Self_Employed == "No",0L,1L)]
test[is.na(Self_Employed), Self_Employed := -999L]

#applicant_income
#coapplicant_incom
train[,.N,Credit_History] #50 NA
train[,.N,Property_Area]
train[,.N,Loan_Status]

train[,ApplicantIncome := log(ApplicantIncome+1L)]
test[,ApplicantIncome := log(ApplicantIncome + 1L)]

train[,CoapplicantIncome := log(CoapplicantIncome + 1L)]
test[,CoapplicantIncome := log(CoapplicantIncome + 1L)]

train[,LoanAmount := log(LoanAmount)]
train[is.na(LoanAmount), LoanAmount := -999]
test[,LoanAmount := log(LoanAmount)]
test[is.na(LoanAmount), LoanAmount := -999]

train[is.na(Loan_Amount_Term),Loan_Amount_Term := -999L]
train[Loan_Amount_Term %in% c(12,36,60,120,84,240,300,480),Loan_Amount_Term := 100L]
test[is.na(Loan_Amount_Term),Loan_Amount_Term := -999L]
test[Loan_Amount_Term %in% c(6,12,36,60,120,84,240,300,350,480),Loan_Amount_Term := 100L]

train[is.na(Credit_History),Credit_History := -999L]
test[is.na(Credit_History),Credit_History := -999L]

#one hot karo
library(dummies)
train <- dummy.data.frame(data = train,names = c("Property_Area"))
test <- dummy.data.frame(data=test,names=c("Property_Area"))
str(train)

setDT(train)
setDT(test)

#label encode target
train[,Loan_Status := ifelse(Loan_Status == "N",0,1)] 
#for XGBoost keep it Loan_Status as integer

#train[,Loan_Status := factor(Loan_Status)]

# #create soem features
# train[,Total_Income := ApplicantIncome + CoapplicantIncome]
# test[,Total_Income := ApplicantIncome + CoapplicantIncome]

a <- train[,.(Loan_ID)]
b <- test[,.(Loan_ID)]
b[a,.N,by=.EACHI,on="Loan_ID"][order(-N)] #for ID in either data


#h2o Tandav
library(h2o)
localH2o <- h2o.init(nthreads = -1)

htrain <- as.h2o(train) 
htest <- as.h2o(test)

df <- h2o.splitFrame(data = htrain,ratios = 0.7,seed = 131)

mtrain <- df[[1]]
mval <- df[[2]]
dim(mtrain)

h2o.table(mtrain$Loan_Status)
h2o.table(mval$Loan_Status)

y <- "Loan_Status"
x <- setdiff(names(mtrain),c(y,"Loan_ID"))
print(x)
#4200476311016

#glm1
glm_fit <- h2o.glm(x=x,
                   y=y,
                   training_frame = mtrain,
                   model_id = "glm_fit",
                   family = "binomial")

#glm2
glm_fit2 <- h2o.glm(x=x,y=y,training_frame = mtrain,model_id = "glm_fit2",
                    family = "binomial",
                    validation_frame = mval,
                    lambda_search = T)

#check & model performance
glm_perf <- h2o.performance(model = glm_fit,newdata = mval)
glm_perf #0.688
glm_perf2 <- h2o.performance(model = glm_fit2,newdata = mval)
glm_perf2 #0.688

#retrieve accuracy scores
h2o.auc(glm_fit2)
h2o.accuracy(glm_perf,thresholds = 0.5)
glm_fit2@model$validation_metrics

#random forest
rf_fit1 <- h2o.randomForest(x=x,y=y,
                            training_frame = mtrain,
                            model_id = "rf_fit1",
                            seed=1)
rf_fit2 <- h2o.randomForest(x=x,y=y,training_frame = mtrain,
                            model_id = "rf_fit2",
                            ntrees = 100,
                            seed = 1)
#compare performance
rf_perf1 <- h2o.performance(rf_fit1,newdata = mval)
rf_perf2 <- h2o.performance(rf_fit2, newdata = mval)

#print
print(rf_perf1) #0.76
print(rf_perf2) #0.75

#third RF
rf_fit3 <- h2o.randomForest(x=x,y=y,
                            training_frame = mtrain,
                            model_id = "rf_fit3",
                            seed = 1,
                            nfolds = 5)

h2o.confusionMatrix(rf_fit3) #validation accuracy
rf_fit3@model$cross_validation_metrics_summary

#GBm
gbm_fit1 <- h2o.gbm(x=x,y=y,
                    training_frame = mtrain,
                    model_id = "gbm_fit1",
                    seed=1)

gbm_fit2 <- h2o.gbm(x=x,y=y,
                    training_frame = mtrain,
                    model_id = "gbm_fit2",
                    ntrees = 500,
                    seed = 1)

gbm_fit3 <- h2o.gbm(x=x,y=y,
                    training_frame = mtrain,
                    model_id = "gbm_fit3",
                    validation_frame = mval,
                    ntrees = 500,
                    score_tree_interval = 5,
                    stopping_rounds = 3,
                    stopping_metric = "misclassification",
                    stopping_tolerance = 0.005,
                    seed = 1)

gbm_perf1 <- h2o.performance(model = gbm_fit1,newdata = mval)
gbm_perf2 <- h2o.performance(model = gbm_fit2,newdata = mval)
gbm_perf3 <- h2o.performance(model = gbm_fit3,newdata = mval)

gbm_perf1 #0.77
gbm_perf2 #0.765
gbm_perf3 #0.775

h2o.scoreHistory(gbm_fit2)


#deep learning
dl_fit1 <- h2o.deeplearning(x=x,y=y,
                            training_frame = mtrain,
                            model_id = "dl_fit1",
                            seed = 1)

dl_fit2 <- h2o.deeplearning(x=x,y=y,training_frame = mtrain,
                            model_id = "dl_fit2",
                            epochs = 20,
                            hidden = c(10,10),
                            stopping_rounds = 0,
                            seed = 1)

dl_fit3 <- h2o.deeplearning(x=x,y=y,training_frame = mtrain,
                            model_id = "dl_fit3",
                            epochs = 20,
                            hidden = c(10,10),
                            score_interval = 1,
                            stopping_rounds = 3,
                            stopping_metric = "misclassification",
                            stopping_tolerance = 0.005,
                            seed = 1)

dl_perf1 <- h2o.performance(dl_fit1,newdata = mval)
dl_perf1 #0.688

dl_perf2 <- h2o.performance(dl_fit2,newdata = mval)
dl_perf2 #0.683

dl_perf3 <- h2o.performance(dl_fit3,newdata = mval)
dl_perf3 #0.683

#GBM and RF
ranfor1 <- h2o.randomForest(x=x,y=y,
                            training_frame = mtrain,
                            validation_frame = mval,
                            model_id = "rf_type1",
                            ntrees = 200,
                            stopping_rounds = 2,
                            stopping_tolerance = 0.01,
                            score_each_iteration = T,
                            seed = 10000)
summary(ranfor1) #0.739

#access validation results
ranfor1@model$validation_metrics

gbm21 <- h2o.gbm(x=x,y=y,training_frame = mtrain,
                 validation_frame = mval,
                 ntrees = 500,
                 seed = 12222,
                 model_id = "gbm_covtype1")

summary(gbm21)
gbm21@model$validation_metrics #0.765

gbm22 <- h2o.gbm(x=x,y=y,
                 training_frame = mtrain,
                 validation_frame = mval,
                 ntrees = 100,
                 learn_rate = 0.2,
                 max_depth = 5,
                 stopping_rounds = 2,
                 stopping_tolerance = 0.01,
                 score_each_iteration = T,
                 model_id = "gbm_wer",
                 seed = 200000)

gbm22@model$validation_metrics #0.785

gbm23 <- h2o.gbm(x=x,y=y,
                 training_frame = mtrain,
                 validation_frame = mval,
                 ntrees = 200,
                 learn_rate = 0.3,
                 max_depth = 5,
                 stopping_rounds = 2,
                 sample_rate = 0.7,
                 col_sample_rate = 0.7,
                 stopping_tolerance = 0.01,
                 score_each_iteration = T,
                 model_id = "gbm_wer",
                 seed = 200000)
gbm23@model$validation_metrics #0.795

rf24 <- h2o.randomForest(x=x,y=y,training_frame = mtrain,
                         validation_frame = mval,
                         ntrees = 200,
                         max_depth = 5,
                         stopping_rounds = 2,
                         stopping_metric = "misclassification",
                         stopping_tolerance = 0.02,
                         score_each_iteration = T,
                         seed = 3000)
rf24@model$validation_metrics #0.693

#hyperparameter Tuning
nfolds <- 5
search_criteria <- list(strategy = "RandomDiscrete",
                        max_runtime_secs = 120)


#GBM Hyper
learn_rate_opt <- seq(0,1,0.01)
max_depth_opt <- seq(1,10,1)
sample_rate_opt <- seq(0.1,0.9,0.1)
col_sample_rate_opt <- seq(0.1,0.9,0.1)
ntrees_opt <- seq(10,500,50)

hyper_params <- list(
        learn_rate = learn_rate_opt,
        max_depth = max_depth_opt,
        sample_rate = sample_rate_opt,
        col_sample_rate = col_sample_rate_opt,
        ntrees = ntrees_opt
)

gbm_grid <- h2o.grid("gbm",x=x,y=y,
                     training_frame = mtrain,
                     #ntrees = 400,
                     seed=1331,
                     model_id="gbm_grid21",
                     nfolds=nfolds,
                     fold_assignment="Modulo",
                     keep_cross_validation_predictions = T,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)
gbm_grid
gbm_gridperf <- h2o.getGrid(grid_id = "Grid_GBM_RTMP_sid_9f95_8_model_R_1477968665203_29928",
                            sort_by = "accuracy",
                            decreasing = T)
print(gbm_gridperf)

#get best gbm model
best_gbm_model <- gbm_gridperf@model_ids[[1]]
best_gbm <- h2o.getModel(best_gbm_model)
best_gbm@model$variable_importances

gbm_models <- lapply(gbm_grid@model_ids,function(model_id)h2o.getModel(model_id))
best_gbm@model$variable_importances


#random forest
nfolds <- 5
search_criteria <- list(strategy = "RandomDiscrete",
                        max_models = 50)

#RF Hyper
mtries_opt <- seq(1,10,1)
max_depth_opt <- seq(1,20,1)
sample_rate_opt <- seq(0.1,0.9,0.1)
col_sample_rate_per_tree_opt <- seq(0.1,0.9,0.1)
ntrees_opt <- seq(50,1000,50)


hyper_params <- list(
        mtries = mtries_opt,
        max_depth = max_depth_opt,
        sample_rate = sample_rate_opt,
        col_sample_rate_per_tree = col_sample_rate_per_tree_opt,
        ntrees = ntrees_opt
)

rf_grid <- h2o.grid("randomForest",x=x,y=y,
                    training_frame = mtrain,
                    seed=101,
                    nfolds=nfolds,
                    fold_assignment="Modulo",
                    keep_cross_validation_predictions=T,
                    hyper_params = hyper_params,
                    search_criteria = search_criteria)

rf_grid_perf <- h2o.getGrid(grid_id = "Grid_DRF_RTMP_sid_ac7f_4_model_R_1477909698283_36952",
                                sort_by = "accuracy",
                            decreasing = T)
rf_grid_perf

best_rf <- h2o.getModel(rf_grid_perf@model_ids[[1]])
best_rf

rf_models <- lapply(rf_grid@model_ids,function(model_id)h2o.getModel(model_id))

#Deep Learning Hyperparameters
activation_opt <- c("Rectifier","RectifierWithDropout","Maxout",
                    "MaxoutWithDropout")

hidden_opt <- list(c(10,10),c(20,10),c(50,50,50),c(20,20,20))
l1_opt <- c(0,1e-3,1e-5)
l2_opt <- c(0,1e-3,1e-5)
hyper_params <- list(activation=activation_opt,
                     hidden=hidden_opt,
                     l1=l1_opt,
                     l2=l2_opt)

dl_grid <- h2o.grid("deeplearning",x=x,y=y,
                    training_frame=mtrain,
                    epochs=15,seed=1,
                    nfolds=nfolds,
                    fold_assignment="Modulo",
                    keep_cross_validation_predictions=T,
                    hyper_params=hyper_params,
                    search_criteria = search_criteria)
dl_models <- lapply(dl_grid@model_ids,function(model_id)h2o.getModel(model_id))

#GLM Hyperparameters
alpha_opt <- seq(0,1,0.1)
lambda_opt <- c(0,1e-7,1e-5,1e-3,1e-1)
hyper_params <- list(alpha=alpha_opt,lambda=lambda_opt)

glm_grid <- h2o.grid("glm",x=x,y=y,
                     training_frame=mtrain,
                     family="binomial",
                     nfolds=nfolds,
                     fold_assignment="Modulo",
                     keep_cross_validation_predictions=T,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)
glm_models <- lapply(glm_grid@model_ids,function(model_id)h2o.getModel(model_id))

#create a list of all base models
models <- c(gbm_models,rf_models,dl_models,glm_models)

#specify defualt glm wrapper
metalearner <- "h2o.glm.wrapper"

#let's stack
library(h2oEnsemble)
stack <- h2o.stack(models = models,
                   response_frame = mtrain[,y],
                   metalearner = metalearner)

perf <- h2o.ensemble_performance(stack,newdata = mval,score_base_models = F)
perf$ensemble

predictmy <- predict.h2o.ensemble(stack,newdata = htest)
xx <- as.data.frame(predictmy$pred$predict)

final_df <- data.table(Loan_ID = test$Loan_ID, Loan_Status = xx)
colnames(final_df)[2] <- "Loan_Status"
final_df[,Loan_Status := ifelse(Loan_Status == 0,"N","Y")]
write.csv(final_df,"1st_Nov_sub.csv",row.names = F)
#stacking sucks = 0.638 score



