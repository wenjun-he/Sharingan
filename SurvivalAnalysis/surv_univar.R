# This script is to used as a wrapper function to do the univariable analysis for survival data
# Author: Hewenjun
# Date: Oct 31 2020

surv_univar <- function(data_frame, outcome_var, col_names_categorical, col_names_continuous){
  # Input:
    # data_frame: Should be dataframe type, categorical variables shoube be factor type
    # outcome_var: Output variables. For example: outcome_var -> c("OS", "Death")
  
  # Return:
    # univariate_res： Results of univariate analysis
    # significant_variable_names: Vector containing significant variables in univariate analysis
  
  ## First specify the packages of interest
  packages = c("ggplot2", "survival","survminer")
  
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
  
  # In cox regression, the outcome variable should be numeric
  data_frame[,outcome_var[2]] <- as.numeric(as.matrix(data_frame[,outcome_var[2]]))
  
  covariates_names <- colnames(data_frame)[-which(colnames(data_frame) %in% outcome_var)]
  
  # Univariate analysis
  outcome_formula <- paste("Surv(", outcome_var[1], ",", outcome_var[2], ")~")
  univ_formulas <- sapply(covariates_names, function(x) as.formula(paste(outcome_formula, x)))
  univ_models <- lapply(univ_formulas, function(x) {coxph(x, data = data_frame)})
  univ_results <- lapply(univ_models,
                         function(x){
                           x <- summary(x)
                           p.value <- signif(x$wald["pvalue"], digits = 5)
                           wald.test <- signif(x$wald["test"], digits = 2)
                           beta <- signif(x$coef[1], digits = 2)
                           HR <- signif(x$coef[2], digits = 2)
                           HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                           HR.confint.upper <- signif(x$conf.int[,"upper .95"], 2)
                           HR <- paste0(HR, "(",
                                        HR.confint.lower, "-", HR.confint.upper, ")")
                           res <- c(beta, HR, wald.test, p.value)
                           names(res) <- c("beta", "HR (95% CI for HR)", "wald.test", "p.value")
                           return(res)
                         })
  res <- as.data.frame(t(as.data.frame(univ_results, check.names = FALSE)))
  res$p.value <- as.numeric(res$p.value)
  
  # P-value of all covariates
  univariate_res <- res[order(res$p.value),]
  # Significant variables
  significant_variable_names <- rownames(res)[res$p.value<0.05]
  
  # KM plot for categorical variables
  plots_suv <- list()
  st2_factor2numeric <- data_frame
  for (i in significant_variable_names) {
    if (i %in% col_names_categorical){
      st2_factor2numeric[,i] <- as.numeric(as.matrix(st2_factor2numeric[,i]))
      fit <- surv_fit(as.formula(paste(outcome_formula, i)), data = st2_factor2numeric)
      plots_suv[[i]] <- ggsurvplot(fit, pval = TRUE, conf.int = TRUE,
                                   risk.table = TRUE,
                                   risk.table.col ="strata",
                                   linetype = "strata",
                                   surv.median.line = "hv",
                                   ggtheme = theme_bw(),
                                   font.tickslab = c(15,"bold"),
                                   font.x = c(15,"bold"),
                                   font.y = c(15,"bold"),
                                   palette = c("#E7B800", "#2E9FDF"))
    }
  }
  jpeg(file = paste(outcome_var[2], "survival_KM.jpeg", sep = "_"), width = 2100, height = 1500)
  print(arrange_ggsurvplots(plots_suv, print = TRUE, ncol = 4, nrow = 4, risk.table.height = 0.2))
  while (!is.null(dev.list()))  dev.off()
  
  # Box plot for continues variables
  data_frame[,outcome_var[2]] <- as.factor(as.matrix(data_frame[,outcome_var[2]]))
  plots_suv <- list()
  for (i in significant_variable_names) {
    if (i %in% col_names_continuous){
      plots_suv[[i]] <- ggplot(data_frame, aes_string(x=outcome_var[2], y=i, fill=outcome_var[2]))+geom_boxplot()+geom_jitter(alpha=0.8,size=0.7)+theme_classic()
    }
  }
  jpeg(file = paste(outcome_var[2], "survival_boxplot.jpeg", sep = "_"), width = 1000, height = 1000)
  print(plot_grid(plotlist =plots_suv))
  while (!is.null(dev.list()))  dev.off()
  
  return(list(univariate_res, significant_variable_names))
}