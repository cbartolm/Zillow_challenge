# Use the bootstrap to calculate significance in the regression coeficients 
library(Metrics)
library(dplyr)
library(xtable)

rm(list = ls())
cat("\014")
set.seed(1)

source('~/Documents/git_hub/Zillow_challenge/load_data.R')
## Modelling

##############################
##### Regression #############
##############################
best_model_interactions = lm(logerror ~ calculatedfinishedsquarefeet + taxamount +
                              taxdelinquencyflag + tract_county + bedroomcnt + calculatedfinishedsquarefeet:taxamount +
                              calculatedfinishedsquarefeet:structuretaxvaluedollarcnt +
                              taxamount:structuretaxvaluedollarcnt + taxamount:lotsizesquarefeet, data= training)

models <- list(best_model_interactions)
models_names = c("best model iteractions")

B = 1000
for (i in 1:B){
  training_sample = sample_n(training, size =nrow(training), replace=TRUE)
  
  best_model_interactions = lm(logerror ~ calculatedfinishedsquarefeet + taxamount +
                                taxdelinquencyflag + tract_county + bedroomcnt + calculatedfinishedsquarefeet:taxamount +
                                calculatedfinishedsquarefeet:structuretaxvaluedollarcnt +
                                taxamount:structuretaxvaluedollarcnt + taxamount:lotsizesquarefeet, data= training_sample)
  
  if (i == 1){
    reg_coef = data.frame(best_model_interactions$coefficients)
  }else{
    temp_reg_coef = data.frame(best_model_interactions$coefficients)
    colnames(temp_reg_coef) = paste(colnames(temp_reg_coef) ,toString(i))
    
    reg_coef <- cbind(reg_coef, temp_reg_coef)
  }
}

names_ = row.names(temp_reg_coef)
rownames(reg_coef) = NULL



CI_quants = c()
CI_normals = c()
for (row in 1:nrow(reg_coef)) {
  x = as.numeric(unname(reg_coef[row,1:ncol(reg_coef)]))
  hist(x, main =names_[row])
  quant1 = quantile(x, 0.025)
  quant2 = quantile(x,0.975)
  
  CI_quants[row] = paste(formatC(quant1,format = "e", digits = 2), "to", formatC(quant2,format = "e", digits = 2), sep=" ")
  CI_normals[row] = paste(formatC(mean(x) - 1.96*sd(x),format = "e", digits = 2), "to", formatC(mean(x) + 1.96*sd(x),format = "e", digits = 2), sep=" ") 
  
  # CI_quants[row] = paste(round(quant1,3), "to", round(quant2,3), sep=" ")
  # CI_normals[row] = paste(round(mean(x) - 1.96*sd(x),3), "to", round(mean(x) + 1.96*sd(x),3), sep=" ") 
  
  cat("N", names_[row], CI_quants[row], "\n")
  cat("Q", names_[row], CI_normals[row], "\n")
}

confidence_interval_summary = data.frame("Name of Variable" = names_, "Normal CI" = CI_normals, "Quantile CI" = CI_quants)
xtable(confidence_interval_summary)



