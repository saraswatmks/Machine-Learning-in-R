path <- "/home/manish/Desktop/Data2017/June/Mercedes/"
setwd(path)

#### load
library(data.table)
library(e1071)

train <- fread("train.csv")
test <- fread("test.csv")


# Set Folds ---------------------------------------------------------------

library(caret)
kf <- createFolds(train$y, k = 4, list=TRUE)

maxL <- as.integer(max(sapply(kf, length)))

#initialize the fold frame
fold_ids <- data.frame(tmp = rep(-1, maxL))


for(i in 1:length(kf)){
  
  row.idx <- kf[[i]]
  
  ids <- train[row.idx]$ID
  if(length(ids) < maxL){
    num_has_needed <- maxL - length(ids)
    ids <- c(ids, rep(NA, num_has_needed))
  }
  
  fold_name <- paste0("Fold_",i)
  
  fold_ids[[fold_name]] <- ids
  
}
fold_ids$tmp <- NULL


binda <- rbindlist(list(train, test), fill = T)

cols_to_conv <- setdiff(colnames(train),c('ID','y'))
binda[,(cols_to_conv) := lapply(.SD, function(x) as.numeric(as.factor(x))-1), .SDcols = cols_to_conv]

train <- binda[1:nrow(train)]
test <- binda[(nrow(train)+1):nrow(binda)]


# Generate features subsets -----------------------------------------------

# this comes from xgboost feature importance - run a model before progressing
imp <- xgb.importance(feature_names = feature.names, model = bst)

fset1 <- c();fset2 <- c();fset3 <- c();fset4 <- c()

for (i in 1:nrow(imp)){
  if(i %% 4 == 1){
    fset1 <- c(fset1, imp$Feature[i])
  }else if(i %% 4 == 2) {
    fset2 <- c(fset2, imp$Feature[i])
  }else if(i %% 4 == 3){
    fset3 <- c(fset3, imp$Feature[i])
  }else {
    fset4 <- c(fset4, imp$Feature[i])
  }
}

# train partitions
dset_tr1 <- cbind(train[, c("ID", "y"),with=F], train[, fset1,with=F])
dset_tr2 <- cbind(train[, c("ID", "y"),with=F], train[, fset2,with=F])
dset_tr3 <- cbind(train[, c("ID", "y"),with=F], train[, fset3,with=F])
dset_tr4 <- cbind(train[, c("ID", "y"),with=F], train[, fset4,with=F])
# test partitions
dset_te1 <- cbind(test[, c("ID"),with=F], test[, fset1,with=F])
dset_te2 <- cbind(test[, c("ID"),with=F], test[, fset2,with=F])
dset_te3 <- cbind(test[, c("ID"),with=F], test[, fset3,with=F])
dset_te4 <- cbind(test[, c("ID"),with=F], test[, fset4,with=F])

#write train partitions
fwrite(dset_tr1, "pipeline/trainP1.csv")
fwrite(dset_tr2, "pipeline/trainP2.csv")
fwrite(dset_tr3, "pipeline/trainP3.csv")
fwrite(dset_tr4, "pipeline/trainP4.csv")
#write test partitions
fwrite(dset_te1, "pipeline/testP1.csv")
fwrite(dset_te2, "pipeline/testP2.csv")
fwrite(dset_te3, "pipeline/testP3.csv")
fwrite(dset_te4, "pipeline/testP4.csv")


# seed list for bagging
seed_list <- c(1, 544, 353, 13, 12345)

num_rounds <- c(1420, 1460, 1420, 1530)
#num_rounds <- c(50,100,150,200)

param <- list(objective = "reg:linear",
              max_depth = 9,
              eta = 0.01,
              subsample = 0.9,
              colsample_bytree = 0.5,
              min_child_weight = 1)



# What the giant loop below does:
# 1. Read the train and test files generated from partitions above
# 2. For every seed, it runs 5 folds CV and rbinds all predictions, in the end
# there will be 4 CV predictions which are then averaged, it creates eval 
# 3. Then for that seed, the model is trained on full train data, and predictions made on test data
# 4. This is repeated for all train and test files. 


for(ii in 1:4){
  
  cat("training with partition",ii,"\n")
  readline()
  train_file <- paste(path,"pipeline/trainP",ii,".csv",sep = "")
  test_file <- paste(path, "pipeline/testP",ii,".csv",sep="")
  model_version <- paste("XGB",paste0("train_P", ii), sep = "_")
  print(model_version)
  
  cat("\nReading ", train_file, "...\n")
  train <- fread(train_file)
  cat("\nReading ",test_file,"...\n")
  test <- fread(test_file)
  cat("\n** # rows in train: ", nrow(train))
  cat("\n** # rows in test: ", nrow(test))
  
  feature.names <- names(train)[!names(train) %in% c("ID","y")]
  
  train[,(feature.names) := lapply(.SD, function(x) as.numeric(x)), .SDcols = feature.names]
  test[,(feature.names) := lapply(.SD, function(x) as.numeric(x)), .SDcols = feature.names]
  
  #initiate empty train and test bag predictions
  train_bag <- matrix(rep(0,4*nrow(train)),ncol = 4)
  test_bag <- matrix(rep(0,2*nrow(test)),ncol=2)

  for(bb in 1:length(seed_list)){
    
    cat("\n** Starting bag:",bb,"**\n")
    set.seed(seed_list[bb])
    
    #train and test meta containers
    eval_matrix = data.frame(ID = numeric(), Fold = numeric(), ground_truth = numeric())
    eval_matrix[model_version] = numeric()
    test_matrix = data.frame(ID = test$ID)
    
    #test matrix
    dtest <- xgb.DMatrix(as.matrix(test[,feature.names,with=F]))
    
    for(jj in 1:length(fold_ids)){
      
      cat("\n--------------------------")
      cat("\n-------Fold: ",jj,"--------")
      cat("\n--------------------------\n")
      
      idx <- fold_ids[[jj]]
      idx <- idx[!is.na(idx)]
      trainingSet <- train[!train$ID %in% idx]
      validationSet <- train[train$ID %in% idx]
      
      print(paste("The length of validation set is",nrow(validationSet)))
      
      dtrain <- xgb.DMatrix(data = as.matrix(trainingSet[,feature.names,with=F]), label = log1p(trainingSet$y))
      dval <- xgb.DMatrix(data = as.matrix(validationSet[,feature.names,with=F]), label = log1p(validationSet$y))
      
      watchlist <- list(OOB = dval, train = dtrain)
      
      rsquare <- function(preds, dtrain){
        
        label <- getinfo(dtrain, 'label')
        err <- R2_Score(preds, label)
        return (list(metric='r2', value = err))
        
      }
      
      bst <- xgb.train(params = param,
                       data = dtrain,
                       nround = as.integer(num_rounds[ii]),
                       print_every_n = 50,
                       watchlist = watchlist,
                       feval = rsquare,
                       maximize = T)
      
      fold_preds <- predict(bst, dval)
      cat("\nFold OOB Score: ", R2_Score(log1p(validationSet$y), fold_preds),"\n")
      df <- data.frame(ID = validationSet$ID, Fold = rep(jj, nrow(validationSet)), ground_truth = validationSet$y, xgb1_preds = exp(fold_preds)-1)
      eval_matrix <- rbind(eval_matrix, df)
      #print(dim(eval_matrix))
      #readline()
      #stop("brute force Loop will end here")
      rm(bst, idx, dtrain, dval)
      gc()
      
      
    }
    
    dtrain <- xgb.DMatrix(data = as.matrix(train[,feature.names,with=F]), label=log1p(train$y))
    watchlist <- list(train = dtrain)
    cat("\nTraining on all data...\n")
    bst <- xgb.train(params = param,
                     data = dtrain,
                     nround = as.integer(num_rounds[ii]),
                     print_every_n = 500,
                     watchlist = watchlist,
                     feval = rsquare)
    
    tpreds <- predict(bst, dtest)
    test_matrix[model_version] = exp(tpreds)-1
    
    train_bag <- train_bag + as.matrix(eval_matrix)
    #cat("\n**This train_bag is inside the loop\n")
    #print(paste0("The dimension of train bag is",dim(train_bag),"\n"))
    test_bag <- test_bag + as.matrix(test_matrix)
    #print(paste0("The dimension of test bag is",dim(test_bag),"\n"))
    rm(bst, eval_matrix, test_matrix)

  }
    
  train_bag <- train_bag/length(seed_list)
  test_bag <- test_bag/length(seed_list)
  
  train_bag <- as.data.frame(train_bag)
  test_bag <- as.data.frame(test_bag)
  #cat("\n**This train_bag is outside the loop before column rename")
  #print(head(train_bag))
  names(train_bag) <- c("ID","Fold","ground_truth","model_version")
  names(test_bag) <-c("ID","model_version")
  
  print(head(train_bag))
  cat("\n")
  print(head(test_bag))
  cat("\n")
  
  train_prediction_file <- paste0(path,"pipeline/",model_version, "_eval.csv")
  test_prediction_file <- paste0(path, "pipeline/",model_version, "_test.csv")
  # save train and test predictions
  fwrite(train_bag, train_prediction_file)
  fwrite(test_bag, test_prediction_file)
  
  rm(train_bag, test_bag)
  gc()
  
  

  #return (train_bag)
  #return (test_bag)
  
  # train_prediction_file <- paste0(train_prediction_path, "./", model_version, "_eval.csv")
  # test_prediction_file <- paste0(test_prediction_path, "./",model_version, "_test.csv")
  # # save train and test predictions
  # write_csv(train_bag, train_prediction_file)
  # write_csv(test_bag, test_prediction_file)
  # 
  # rm(train_bag, test_bag)
  # gc()
  
}



