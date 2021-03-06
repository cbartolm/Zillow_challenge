######################################
# Project Milestone
# Bernardo Casares, Cristian Bartolome
######################################

library(Metrics)
library(dplyr)

rm(list = ls())
set.seed(1)


# Load Data
data_set = read.csv("../datasets_stanford/merged_train_2016.csv")

MakeFeaturesCorrectType = function(data_set){
  # Make some features the correct type
  data_set$hashottuborspa <- ifelse(data_set$hashottuborspa == 'true', 1, 0)
  data_set$fireplaceflag <- ifelse(data_set$fireplaceflag == 'true', 1, 0)
  data_set$taxdelinquencyflag <- ifelse(data_set$taxdelinquencyflag == 'Y', 1, 0)
  data_set$propertycountylandusecode <- as.numeric(as.factor(data_set$propertycountylandusecode))
  data_set$propertyzoningdesc <- as.numeric(as.factor(data_set$propertyzoningdesc))
  
  data_set <- data_set %>% mutate(census = as.character(rawcensustractandblock), tract_county = substr(census,1,4), 
                                  tract_number = substr(census,5,11), tract_block = substr(census,12,length(census)))
  return(data_set)
}

RemoveUneccessaryData = function(data_set){
  # Remove unnecessary columns or excesive na values
  data_set = data_set[colSums(is.na(data_set)|data_set == '')/nrow(data_set) < 0.5]
  data_set = data_set[, !names(data_set) %in% c('parcelid', 'transactiondate')] # Unecessary 
  data_set = data_set[vapply(data_set, function(x) length(unique(x)) > 1, logical(1L))] # Same values in col
  # The code below removed linearly dependant (i.e calculatedfinishedsquarefeet and finishedsquarefeet12)
  data_set = data_set[, !names(data_set) %in% c('finishedsquarefeet12', 'calculatedbathnbr', "fullbathcnt",
                                                "landtaxvaluedollarcnt", "regionidcounty", "fips", "censustractandblock")]
  # Remove categorical variables with only one predominant variable. For example: only one instance of 
  # category and all the rest of a different category
  data_set = data_set[, !names(data_set) %in% c('roomcnt', "unitcnt")]
  # Remove census data that could be used later for adding additional data
  data_set = data_set[, !names(data_set) %in% c("census", "tract_number","tract_block", "latitude", "longitude")]
  # Remove the column below because it was already split previously
  data_set = data_set[, !names(data_set) %in% c("rawcensustractandblock")]
  
  return(data_set)
}

CreateNoneFactor = function(data_set_name){
  # Get levels and add "None"
  data_set_name = factor(data_set_name)
  levels <- levels(data_set_name)
  levels[length(levels) + 1] <- "None"
  data_set_name <- factor(data_set_name, levels = levels)
  data_set_name[is.na(data_set_name)] <- "None"
  return(data_set_name)
}

KeepFactorGreater = function(data_set, data_set_name, greater_than = 20){
  keep <- levels(data_set_name)[table(data_set_name) > greater_than]
  data_set <- data_set[data_set_name %in% keep,]
  return(data_set)
}

ColapseFactor = function(data_set_name, less_than){
  data_set_name = factor(data_set_name)
  levels <- levels(data_set_name)
  levels[length(levels) + 1] <- "Collapsed"
  
  data_set_name <- factor(data_set_name, levels = levels)
  change <- levels(data_set_name)[table(data_set_name) < less_than]
  data_set_name[data_set_name %in% change] <- "Collapsed"
  return(data_set_name)
}

ChangeValueFactorGreatherThan = function(data_set_name, value){
  data_set_name[data_set_name > value] <- "Greather_than"
  data_set_name = factor(data_set_name)
  return(data_set_name)
}

ChangeValueFactorLessThan = function(data_set_name, value){
  data_set_name[data_set_name < value] <- "Less_Than"
  data_set_name = factor(data_set_name)
  return(data_set_name)
}

ChangeZipToClosest = function(data_set_name, less_than){
  temp = data_set_name[!is.na(data_set_name)]
  
  data_set_name <- factor(data_set_name)
  change <- levels(data_set_name)[table(data_set_name) < less_than]
  temp = temp[!(temp %in% change)]
  for (value in change){
    x = as.numeric(value)
    minimum_index = which(abs(temp-x)==min(abs(temp-x)))
    data_set_name[data_set_name %in% value] <- temp[minimum_index[1]]
  }
  return(data_set_name)
}

ConvertCategoricalVariablesToFactors = function(data_set){
  # Create categorical variables
  data_set$taxdelinquencyflag = factor(data_set$taxdelinquencyflag)
  
  data_set$buildingqualitytypeid = CreateNoneFactor(data_set$buildingqualitytypeid)
  data_set = KeepFactorGreater(data_set=data_set, data_set_name=data_set$buildingqualitytypeid)
  
  data_set$heatingorsystemtypeid = CreateNoneFactor(data_set$heatingorsystemtypeid)
  data_set = KeepFactorGreater(data_set=data_set, data_set_name=data_set$heatingorsystemtypeid)
  
  data_set$propertycountylandusecode = ColapseFactor(data_set_name=data_set$propertycountylandusecode, less_than=100)

  data_set$bathroomcnt = ChangeValueFactorGreatherThan(data_set_name=data_set$bathroomcnt, value=6)
  
  data_set$regionidcity = ColapseFactor(data_set_name=data_set$regionidcity, less_than=50)
  data_set$regionidcity = CreateNoneFactor(data_set$regionidcity)
  
  data_set$regionidzip = ChangeZipToClosest(data_set_name =data_set$regionidzip, less_than=20)
  data_set$regionidzip = CreateNoneFactor(data_set$regionidzip)
  
  data_set$bedroomcnt = ChangeValueFactorGreatherThan(data_set_name=data_set$bedroomcnt, value=8)
  
  data_set$propertyzoningdesc = ChangeValueFactorGreatherThan(data_set_name=data_set$propertyzoningdesc, value=1)
  
  data_set$yearbuilt = ChangeValueFactorLessThan(data_set$yearbuilt, value=1935)
  data_set$yearbuilt = CreateNoneFactor(data_set$yearbuilt)
  
  data_set = droplevels(data_set)
  return(data_set)
}

getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

FillInMissingValues = function(data_set){
  data_set$lotsizesquarefeet[is.na(data_set$lotsizesquarefeet)] <- mean(data_set$lotsizesquarefeet, na.rm=TRUE)
  data_set$taxamount[is.na(data_set$taxamount)] <- mean(data_set$taxamount, na.rm=TRUE)
  data_set$calculatedfinishedsquarefeet[is.na(data_set$calculatedfinishedsquarefeet)] <- mean(data_set$calculatedfinishedsquarefeet, na.rm=TRUE)
  data_set$structuretaxvaluedollarcnt[is.na(data_set$structuretaxvaluedollarcnt)] <- mean(data_set$structuretaxvaluedollarcnt, na.rm=TRUE)
  
  # data_set$regionidcity[is.na(data_set$regionidcity)] <- getMode(data_set$regionidcity)
  
  return(data_set)
}

DataPreparation = function(data_set){
    data_set = FillInMissingValues(data_set=data_set)
    data_set = MakeFeaturesCorrectType(data_set=data_set)
    data_set = RemoveUneccessaryData(data_set=data_set)
    data_set = ConvertCategoricalVariablesToFactors(data_set=data_set)

    # First attempt, remove all missing data
    # data_set = data_set[complete.cases(data_set), ]
  
  return(data_set)
}


data_set = DataPreparation(data_set=data_set)

SplitData = function(data_set, trainin_value = 0.8){
  # Split into training and validation
  in.train = sample(nrow(data_set), size = as.integer(nrow(data_set)*trainin_value))
  training = data_set[in.train, ]
  validation = data_set[-in.train, ]
  split_data <- list("training"=training, "validation"=validation)
  return(split_data)
}

split_data = SplitData(data_set=data_set)
training = split_data$training
validation = split_data$validation

##############################
##### Regression #############
##############################

## Models ####
base_line = lm(logerror ~ calculatedfinishedsquarefeet, data=training) # most correlated variable
all_coeff = lm(logerror ~ ., data=training)
best_model_step_wise = lm(logerror ~ calculatedfinishedsquarefeet + taxamount + propertycountylandusecode +
                            taxvaluedollarcnt + taxdelinquencyflag + tract_county + bedroomcnt, data=training)
bes_model_interactions = lm(logerror ~ calculatedfinishedsquarefeet + taxamount + propertycountylandusecode +
                              taxvaluedollarcnt + taxdelinquencyflag + tract_county + bedroomcnt + calculatedfinishedsquarefeet:taxamount +
                              calculatedfinishedsquarefeet:taxvaluedollarcnt + calculatedfinishedsquarefeet:structuretaxvaluedollarcnt +
                              taxamount:taxvaluedollarcnt + taxvaluedollarcnt:structuretaxvaluedollarcnt +
                              taxamount:structuretaxvaluedollarcnt + taxamount:lotsizesquarefeet, data= training)


models <- list(base_line, all_coeff, best_model_step_wise, bes_model_interactions)
models_names = c("base_line", "all coefficients", "best model step wise", "best model iteractions")

i = 1
for (model in models){
  training_error = rmse(training$logerror, model$fitted.values)
  val_error = rmse(validation$logerror, predict(model, validation))
  cat("The training error for the", models_names[i], "model is:", training_error, "\n")
  cat("The validation error for the", models_names[i], "model is:", val_error, "\n")
  i = i+1
}


# # Reduce training to calculate interactions
# training = training[, names(training) %in% c("logerror", "calculatedfinishedsquarefeet", "lotsizesquarefeet", 
#                                              "structuretaxvaluedollarcnt", "taxvaluedollarcnt", "taxamount")]
# 
# # Stepwise regression
# fm.lower = lm(logerror ~ 1, data = training)
# fm.upper = lm(logerror ~ .:., data = training)
# 
# step(fm.lower, scope = list(lower = fm.lower, upper = fm.upper), direction = "forward")

##############################
##### Classification #########
##############################


training$logerror[training$logerror > 0] = 1
training$logerror[training$logerror < 0] = 0

validation$logerror[validation$logerror > 0] = 1
validation$logerror[validation$logerror < 0] = 0
 
### Model ####
base_line = glm(logerror ~ calculatedfinishedsquarefeet, data=training) # most correlated variable
all_coeff = glm(logerror ~ ., data=training)
best_model_step_wise = glm(logerror ~ bathroomcnt + regionidcity + propertycountylandusecode +
                           calculatedfinishedsquarefeet + taxamount + taxvaluedollarcnt +
                           structuretaxvaluedollarcnt + taxdelinquencyflag + bedroomcnt, data=training)
bes_model_interactions = glm(logerror ~ bathroomcnt + regionidcity + propertycountylandusecode +
                             calculatedfinishedsquarefeet + taxamount + taxvaluedollarcnt +
                             structuretaxvaluedollarcnt + taxdelinquencyflag + bedroomcnt +
                             calculatedfinishedsquarefeet:taxvaluedollarcnt + 
                             calculatedfinishedsquarefeet:taxamount + taxamount:lotsizesquarefeet + 
                             taxvaluedollarcnt:lotsizesquarefeet + taxamount:taxvaluedollarcnt,
                             data=training)



 
models <- list(base_line, all_coeff, best_model_step_wise, bes_model_interactions)
models_names = c("base_line", "all coefficients", "best model step wise", "best model iteractions")

i = 1
for (model in models){
  training_predictions = model$fitted.values > 0.5
  is_missclassified_training = training_predictions != training$logerror
  zero_one_loss_training = 1/nrow(training)*sum(is_missclassified_training)

  validation_predictions = predict(model,validation) > 0.5
  is_missclassified_validation = validation_predictions != validation$logerror
  zero_one_loss_validation = 1/nrow(validation)*sum(is_missclassified_validation)


  cat("Using logistic regression, the zero one loss for the", models_names[i], "model is on the testing set is", zero_one_loss_training, "\n")
  cat("Using logistic regression, the zero one loss for the", models_names[i], "model is on the validation set is", zero_one_loss_validation, "\n")
  i = i+1
}

# Reduce training to calculate interactions
# training = training[, names(training) %in% c("logerror", "calculatedfinishedsquarefeet", "lotsizesquarefeet",
#                                              "structuretaxvaluedollarcnt", "taxvaluedollarcnt", "taxamount")]
# 
# # Stepwise regression
# fm.lower = glm(logerror ~ 1, data = training)
# fm.upper = glm(logerror ~ .:., data = training)
# 
# step(fm.lower, scope = list(lower = fm.lower, upper = fm.upper), direction = "forward")