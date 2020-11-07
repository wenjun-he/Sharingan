# This script is to used as a wrapper function to do the mutivariate analysis for survival data
# Author: Hewenjun
# Date: Oct 31 2020

surv_multivar <- function(data_frame, outcome_var, significant_variable_names){
  # Stepwise for model selection when containing all significant variables in univiate analysis.
  library("glmnet")
  library("survival")
  
  # In cox regression, the outcome variable should be numeric
  data_frame[,outcome_var[2]] <- as.numeric(as.matrix(data_frame[,outcome_var[2]]))
  
  # Cox regression
  outcome_formula <- paste("Surv(", outcome_var[1], ",", outcome_var[2], ")~")
  fmla <- as.formula(paste(outcome_formula,paste(significant_variable_names, collapse = "+")))
  res.cox <- coxph(fmla, data = data_frame)
  res.cox.stepped=step(res.cox)
  
  # Lasso algorithm to get the optimal lambda value
  # Lasso Cox regression
  cv.fit <- cv.glmnet(data.matrix(data_frame[,covariates_names]), Surv(data_frame[,outcome_var[1]], data_frame[,outcome_var[1]]), family="cox")
  plot(cv.fit)
  lasso_cox_fit <- glmnet(data.matrix(data_frame[,covariates_names]), Surv(data_frame[,outcome_var[1]], data_frame[,outcome_var[1]]), family="cox")
  Coefficients <- coef(lasso_cox_fit, s = cv.fit$lambda.min)
  Active.Index <- which(Coefficients != 0)
  Active.Coefficients <- Coefficients[Active.Index]
  Lasso_res_variates <- covariates_names[Active.Index]
  
  return(res.cox.stepped, Lasso_res_variates)
}