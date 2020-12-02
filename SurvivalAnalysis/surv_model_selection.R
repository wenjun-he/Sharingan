# This script is to used as a wrapper function to do the model comparision analysis for survival data
# Author: Hewenjun
# Date: Nov 28 2020


model_compare <- function(data_used, outcome_var, significant_variable_names){
  
  # Input:
  # data_used: Should be dataframe type, categorical variables shoube be factor type
  # outcome_var: Output variables. For example: outcome_var -> c("OS", "Death")
  # significant_variable_names: A vector containing variable names which is statistical significant in univariate analysis.
  
  # Return:
  
  ## First specify the packages of interest
  packages = c("survival","glmnet")
  
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
  
  ## Cox model in complete cases
  seeds<-c(1:50)
  
  c_index_res_train <- c()
  c_index_res_test <- c()
  selected_variables <- c()
  for (k in seeds){
    set.seed(k)
    index <- sample(1:nrow(data_used), round(0.3*nrow(data_used)))
    test_set <- data_used[index,]
    train_set<- data_used[-index,]
    outcome_formula <- paste("Surv(", outcome_var[1], ",", outcome_var[2], ")~")
    fmla <- as.formula(paste(outcome_formula,paste(significant_variable_names, collapse = "+")))
    
    train_set[,outcome_var[2]] <- as.numeric(as.matrix(train_set[,outcome_var[2]]))
    test_set[,outcome_var[2]] <- as.numeric(as.matrix(test_set[,outcome_var[2]]))
    
    res.cox <- coxph(fmla, data = train_set, x = TRUE)
    res.cox.stepped=step(res.cox, trace = 0)
    
    for(j in names(res.cox.stepped$coefficients)){
      selected_variables <- c(selected_variables, j)
    }
    
    train_pred <- predict(res.cox.stepped, newdata = train_set)
    test_pred <- predict(res.cox.stepped, newdata = test_set)
    train_c_index <- survConcordance(as.formula(paste(outcome_formula, "train_pred")), data = train_set)$concordance
    test_c_index <- survConcordance(as.formula(paste(outcome_formula, "test_pred")), data = test_set)$concordance
    
    c_index_res_train <- c(c_index_res_train,round(train_c_index,3))
    c_index_res_test <- c(c_index_res_test,round(test_c_index,3))
  }
  coxph_performance <- data.frame("c-index-train" = c_index_res_train, "c-index-test" = c_index_res_test)
  
  ## Bar plot for selected variables
  jpeg(file = paste(outcome_var[2], "coxph-selected-variable-in-50trials.jpg", sep = "_"), width = 1000, height = 500)
  print(ggplot(as.data.frame(selected_variables),aes_string(x='selected_variables')) + geom_bar(aes_string(fill = 'selected_variables')) + theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5)))
  while (!is.null(dev.list()))  dev.off()
  
  ## Box plot for c-index in train_set and test_set
  coxph_performance <- melt(coxph_performance)
  jpeg(file = paste(outcome_var[2], "coxph-c-index-summary-in-50trials.jpg", sep = "_"), width = 1000, height = 500)
  print(ggplot(data = coxph_performance, aes(x=variable, y=value)) + geom_boxplot(aes(fill=variable)) + geom_jitter(alpha=0.8,size=0.7)+theme_classic())
  while (!is.null(dev.list()))  dev.off()
  
  
  ## Lasso model in 50 trial
  c_index_res_train <- c()
  c_index_res_test <- c()
  selected_variables <- c()
  
  for (k in seeds){
    set.seed(k)
    index <- sample(1:nrow(data_used), round(0.3*nrow(data_used)))
    test_set <- data_used[index,]
    train_set<- data_used[-index,]
    
    covariates_names <- colnames(data_used)[-which(colnames(data_used) %in% outcome_var)]
    cv.fit <- cv.glmnet(data.matrix(train_set[,covariates_names]), 
                        Surv(as.numeric(as.matrix(train_set[,outcome_var[1]])), as.numeric(as.matrix(train_set[,outcome_var[2]]))), 
                        family="cox", 
                        type.measure="C" )
    lasso_cox_fit <- glmnet(data.matrix(train_set[,covariates_names]), 
                            Surv(as.numeric(as.matrix(train_set[,outcome_var[1]])), as.numeric(as.matrix(train_set[,outcome_var[2]]))),
                            family="cox")
    Coefficients <- coef(lasso_cox_fit, s = cv.fit$lambda.min)
    Active.Index <- which(Coefficients != 0)
    Active.Coefficients <- Coefficients[Active.Index]
    for(j in covariates_names[Active.Index]){
      selected_variables <- c(selected_variables, j)
    }
    
    train_pred <- predict(lasso_cox_fit, s = cv.fit$lambda.min, newx = data.matrix(train_set[,covariates_names]))
    test_pred <- predict(lasso_cox_fit, s = cv.fit$lambda.min, newx = data.matrix(test_set[,covariates_names]))

    train_set[,outcome_var[2]] <- as.numeric(as.matrix(train_set[,outcome_var[2]]))
    test_set[,outcome_var[2]] <- as.numeric(as.matrix(test_set[,outcome_var[2]]))
    
    train_c_index <- survConcordance(as.formula(paste(outcome_formula, "train_pred")), data = train_set)$concordance
    test_c_index <- survConcordance(as.formula(paste(outcome_formula, "test_pred")), data = test_set)$concordance
    
    c_index_res_train <- c(c_index_res_train,round(train_c_index,3))
    c_index_res_test <- c(c_index_res_test,round(test_c_index,3))
    
  }
  
  ## Bar plot for selected variables
  lasso_performance <- data.frame("c-index-train" = c_index_res_train, "c-index-test" = c_index_res_test)
  jpeg(file = paste(outcome_var[2], "lasso-selected-variable-in-50trials.jpg", sep = "_"), width = 1000, height = 500)
  print(ggplot(as.data.frame(selected_variables),aes_string(x='selected_variables')) + geom_bar(aes_string(fill = 'selected_variables'))+theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5)))
  while (!is.null(dev.list()))  dev.off()
  
  ## Box plot for c-index in train_set and test_set
  lasso_performance <- melt(lasso_performance)
  jpeg(file = paste(outcome_var[2], "lasso-c-index-summary-in-50trials.jpg", sep = "_"), width = 1000, height = 500)
  print(ggplot(data = lasso_performance, aes(x=variable, y=value)) + geom_boxplot(aes(fill=variable)) + geom_jitter(alpha=0.8,size=0.7)+theme_classic())
  while (!is.null(dev.list()))  dev.off()
}



