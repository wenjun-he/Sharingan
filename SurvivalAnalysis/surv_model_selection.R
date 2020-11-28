# This script is to used as a wrapper function to do the model comparision analysis for survival data
# Author: Hewenjun
# Date: Nov 28 2020

model_compare <- function(data_frame, outcome_var, significant_variable_names){
  
  # Input:
  # data_frame: Should be dataframe type, categorical variables shoube be factor type
  # outcome_var: Output variables. For example: outcome_var -> c("OS", "Death")
  # significant_variable_names: A vector containing variable names which is statistical significant in univariate analysis.
  
  # Return:
  
  ## First specify the packages of interest
  packages = c("mlr","magicfor")
  
  ## Now load or install&load all
  package.check <- lapply(
    packages,
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
      }
    }
  )
  
  #######
  library("readxl")
  data_used <- read_excel("/hewenjun/rstudio_work/zhenweipeng_version2/training-1-20201104-OS.xlsx")
  
  for (j in colnames(data_used)) {
    data_used[,j] <- as.numeric(as.matrix(data_used[,j]))
  }
  for (k in 1:length(colnames(data_used))){
    colnames(data_used)[k] <- gsub("-", "_", colnames(data_used)[k])
    colnames(data_used)[k] <- gsub("\\...", "col", colnames(data_used)[k])
  }
  #######
  
  ## Cox model in complete cases
  random_tune <- makeTuneControlRandom(maxit = 1L)
  rdesc = makeResampleDesc( "CV", iters = 10, stratify = TRUE ) #"Holdout")
  
  seeds<-c(1:50)
  c_index_res_train <- c()
  c_index_res_test <- c()
  for (k in seeds){
    set.seed(k)
    index <- sample(1:nrow(data_used), round(0.3*nrow(data_used)))
    test_set <- data_used[index,]
    train_set<- data_used[-index,]
    task <- makeSurvTask(data = train_set,target=c('OS','Death'))
    cox.lrn <- makeLearner(cl="surv.coxph", 
                           predict.type="response")
    modcox = train(cox.lrn, task) 
    train_pred<-predict(modcox, newdata = train_set)
    train_p<-performance(train_pred, measures = list(cindex)) 
    test_pred<-predict(modcox, newdata = test_set)
    test_p<-performance(test_pred, measures = list(cindex)) 
    #browser()
    c_index_res_train <- c(c_index_res_train,round(train_p,3))
    c_index_res_test <- c(c_index_res_test,round(test_p,3))
  }
  performance <- data.frame("c-index-train" = c_index_res_train, "c-index-test" = c_index_res_test)
  return(performance)
}

res <- model_compare()
summary(res)