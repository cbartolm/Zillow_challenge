######################################
# MS&E266: Mini-Project Milestone 3
# Bernardo Casares, Cristian Bartolome
######################################

library(Metrics)
library(dplyr)

rm(list = ls())
set.seed(1)

### Prediction on Train and Test-set

source('~/Documents/git_hub/Zillow_challenge/load_data.R')

## Modelling

##############################
##### Regression #############
##############################

## Models ####
base_line = lm(logerror ~ calculatedfinishedsquarefeet, data=training) # most correlated variable
all_coeff = lm(logerror ~ ., data=training)

best_model_step_wise = lm(logerror ~ calculatedfinishedsquarefeet + taxamount +
                          taxdelinquencyflag + tract_county + bedroomcnt, data=training)
bes_model_interactions = lm(logerror ~ calculatedfinishedsquarefeet + taxamount +
                              taxdelinquencyflag + tract_county + bedroomcnt + calculatedfinishedsquarefeet:taxamount +
                              calculatedfinishedsquarefeet:structuretaxvaluedollarcnt +
                              taxamount:structuretaxvaluedollarcnt + taxamount:lotsizesquarefeet, data= training)

models <- list(base_line, all_coeff, best_model_step_wise, bes_model_interactions)
models_names = c("base_line", "all coefficients", "best model step wise", "best model iteractions")

# Compute metrics Train/Dev

i = 1
for (model in models){
  training_error = rmse(training$logerror, model$fitted.values)
  val_error = rmse(validation$logerror, predict(model, validation))
  test_error = rmse(testing$logerror, predict(model, testing))
  cat("The training error for the", models_names[i], "model is:", training_error, "\n")
  cat("The validation error for the", models_names[i], "model is:", val_error, "\n")
  cat("The test error for the", models_names[i], "model is:", test_error, "\n")
  i = i+1
}


##############################
##### Classification #########
##############################

## Some data preprocessing
training$logerror[training$logerror > 0] = 1
training$logerror[training$logerror < 0] = 0

validation$logerror[validation$logerror > 0] = 1
validation$logerror[validation$logerror < 0] = 0

testing$logerror[testing$logerror > 0] = 1
testing$logerror[testing$logerror < 0] = 0

### Model ####
base_line = glm(logerror ~ calculatedfinishedsquarefeet, data=training) # most correlated variable
all_coeff = glm(logerror ~ ., data=training)
best_model_step_wise = glm(logerror ~ bathroomcnt + regionidcity + propertycountylandusecode +
                             calculatedfinishedsquarefeet + taxamount +
                             structuretaxvaluedollarcnt + taxdelinquencyflag + bedroomcnt, data=training)
bes_model_interactions = glm(logerror ~ bathroomcnt + regionidcity + propertycountylandusecode +
                               calculatedfinishedsquarefeet + taxamount +
                               structuretaxvaluedollarcnt + taxdelinquencyflag + bedroomcnt +
                               calculatedfinishedsquarefeet:taxamount + taxamount:lotsizesquarefeet,
                             data=training)


models <- list(base_line, all_coeff, best_model_step_wise, bes_model_interactions)
models_names = c("base_line", "all coefficients", "best model step wise", "best model iteractions")

# Compute metrics Train/Dev and Test Set

i = 1
for (model in models){
  training_predictions = model$fitted.values > 0.5
  is_missclassified_training = training_predictions != training$logerror
  zero_one_loss_training = 1/nrow(training)*sum(is_missclassified_training)

  validation_predictions = predict(model,validation) > 0.5
  is_missclassified_validation = validation_predictions != validation$logerror
  zero_one_loss_validation = 1/nrow(validation)*sum(is_missclassified_validation)

  test_predictions = predict(model,testing) > 0.5
  is_missclassified_testing = test_predictions != testing$logerror
  zero_one_loss_testing = 1/nrow(testing)*sum(is_missclassified_testing)


  cat("Using logistic regression, the zero one loss for the", models_names[i], "model is on the testing set is", zero_one_loss_training, "\n")
  cat("Using logistic regression, the zero one loss for the", models_names[i], "model is on the validation set is", zero_one_loss_validation, "\n")
  cat("Using logistic regression, the zero one loss for the", models_names[i], "model is on the test set is", zero_one_loss_testing, "\n")
  i = i+1
}

