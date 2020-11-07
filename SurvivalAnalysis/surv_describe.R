# This script is to used as a wrapper function to do the descriptive analysis for survival data
# Author: Hewenjun
# Date: Oct 31 2020

surv_describe <- function(data_frame, outcome_var){
  # For example: outcome_var -> c("OS", "Death")
  # data_frame: should be dataframe type, not has NAs and has one group outcome variable, and all the data should be numeric
  
  ## First specify the packages of interest
  packages = c("summarytools", "ggplot2","reshape2", "pheatmap","cowplot")
  
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

  # Specify Continuous and Cateogorical Variable
  col_names_continuous <- c()
  col_names_categorical <- c()
  
  # variable with the number of different value less then 20% of sample size is considered to be categorical variable
  for (j in colnames(data_frame)) {
    res_table <- table(data_frame[,j])
    if (length(res_table) > (dim(data_frame)[1] * 0.2)){
      col_names_continuous <- c(col_names_continuous,j)
    }else if (length(res_table) >= 2){
      col_names_categorical <- c(col_names_categorical,j)
    }
  }
  
  # Make sure the categorical variables in data_frame are factor type
  for (i in col_names_categorical) {
    data_frame[,i] <- as.factor(as.matrix(data_frame[,i])) # Should add as.matrix or encounter error
  }
  continue_data <- data_frame[,col_names_continuous]
  cateogorical_data <- data_frame[,col_names_categorical]
  
  # Split outcome variable and covariates
  covariates_names <- colnames(data_frame)[-which(colnames(data_frame) %in% outcome_var)]
  covariates_data <- data_frame[,covariates_names]
  outcome_data <- data_frame[,outcome_var]
  
  # Summary outcome variables
  dfsum_outcome <- dfSummary(outcome_data)
  # Summary covariates
  dfsum_cov <- dfSummary(covariates_data)
  
  # Plot the distribution of continus variable
  plots <- NULL
  for (i in colnames(continue_data)) {
    plots[[i]] <- ggplot(continue_data, aes_string(x=i))+ geom_density(fill = "lightblue", alpha=0.5, show.legend = F)+ theme(axis.text = element_text(size = 20), axis.title = element_text(size=100, face = "bold")) + ylab("")
  }
  jpeg(file = paste(outcome_var[2], "continus_variable_distribution.jpg", sep = "_"), width = 10000, height = 10000)
  print(plot_grid(plotlist = plots))
  while (!is.null(dev.list()))  dev.off()

  # Boxplots for continuous variables
  box_data <- melt(continue_data)
  jpeg(file = paste(outcome_var[2], "continus_variable_boxplot.jpg", sep = "_"), width = 2000, height = 1000)
  print(ggplot(box_data,aes(variable,value))+geom_boxplot(fill="green", outlier.colour = "red", outlier.shape = 8, outlier.size = 4)+theme_classic()+ theme(axis.text = element_text(size = 11,face = "bold"), axis.title = element_text(size=20, face = "bold")))
  while (!is.null(dev.list()))  dev.off()

  # Correlation map for continuous variables
  cor_matrix <- cor(continue_data)
  jpeg(file = paste(outcome_var[2], "correlation_map.jpg", sep = "_"), width = 1000, height = 1000)
  print(pheatmap(cor_matrix, scale = "row",clustering_distance_rows = "correlation"))
  while (!is.null(dev.list()))  dev.off()

  # Bar plots of cateogorical variables
  plots <- NULL
  for (i in colnames(cateogorical_data)) {
    plots[[i]] <- ggplot(cateogorical_data, aes_string(x = i)) + geom_bar(aes_string(fill = i)) + theme(axis.text = element_text(size = 20), axis.title = element_text(size=100, face = "bold")) + ylab("")
  }
  jpeg(file = paste(outcome_var[2], "categorical_variable_barplot.jpg", sep = "_"), width = 10000, height = 10000)
  print(plot_grid(plotlist = plots))
  while (!is.null(dev.list()))  dev.off()
  
  return(list(data_frame,dfsum_outcome,dfsum_cov))
}