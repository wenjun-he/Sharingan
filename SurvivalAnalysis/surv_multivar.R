# This script is to used as a wrapper function to do the mutivariate analysis for survival data
# Author: Hewenjun
# Date: Oct 31 2020

surv_multivar <- function(data_frame, outcome_var, significant_variable_names){
  
  # Input:
    # data_frame: Should be dataframe type, categorical variables shoube be factor type
    # outcome_var: Output variables. For example: outcome_var -> c("OS", "Death")
    # significant_variable_names: A vector containing variable names which is statistical significant in univariate analysis.
  
  # Return:
    # res.cox.stepped: Result of step-wise model selection
    # Lasso_res_variates: Variable selected by Lasso algorithm
    # coef_lasso_cox: Coefficient of Lasso regression
  
  ## First specify the packages of interest
  packages = c("glmnet", "survival")
  
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
  
  # Step-wise for model selection when containing all significant variables in univariate analysis.
  # In cox regression, the outcome variable should be numeric
  data_frame[,outcome_var[2]] <- as.numeric(as.matrix(data_frame[,outcome_var[2]]))
  
  # Cox regression
  outcome_formula <- paste("Surv(", outcome_var[1], ",", outcome_var[2], ")~")
  fmla <- as.formula(paste(outcome_formula,paste(significant_variable_names, collapse = "+")))
  res.cox <- coxph(fmla, data = data_frame)
  res.cox.stepped=step(res.cox, trace = 0)
  
  # Lasso algorithm to get the optimal lambda value
  # In Lasso cox regression, we would like to use all the covariables in regression
  covariates_names <- colnames(data_frame)[-which(colnames(data_frame) %in% outcome_var)]
  
  # Lasso Cox regression
  set.seed(12)
  cv.fit <- cv.glmnet(data.matrix(data_frame[,covariates_names]), 
                      Surv(as.numeric(as.matrix(data_frame[,outcome_var[1]])), as.numeric(as.matrix(data_frame[,outcome_var[2]]))), 
                      family="cox", 
                      type.measure="C" )
  lasso_cox_fit <- glmnet(data.matrix(data_frame[,covariates_names]), 
                          Surv(as.numeric(as.matrix(data_frame[,outcome_var[1]])), as.numeric(as.matrix(data_frame[,outcome_var[2]]))),
                          family="cox")
  Coefficients <- coef(lasso_cox_fit, s = cv.fit$lambda.min)
  Active.Index <- which(Coefficients != 0)
  Active.Coefficients <- Coefficients[Active.Index]
  
  # Variables selected by Lasso Cox in C-index 
  Lasso_res_variates <- covariates_names[Active.Index]
  
  # Coefficient of Lasso Cox in optimal lambda
  coef_lasso_cox <- predict(cv.fit,type='coefficients',s=cv.fit$lambda.min)
  
  # Coefficient change as L1 norm in Lasso Cox
  jpeg(file = paste(outcome_var[2], "lasso_coefficient_change.jpeg", sep = "_"), width = 1000, height = 700)
  plot(cv.fit$glmnet.fit,xvar = "lambda",label = T)
  abline(v=log(c(cv.fit$lambda.min,cv.fit$lambda.1se)),lty=2)
  while (!is.null(dev.list()))  dev.off()
  
  # Change of C-index
  jpeg(file = paste(outcome_var[2], "lasso_change_of_C_index.jpeg", sep = "_"), width = 1000, height = 700)
  plot(cv.fit)
  while (!is.null(dev.list()))  dev.off()
  
  return(list(res.cox.stepped, Lasso_res_variates, coef_lasso_cox))
}