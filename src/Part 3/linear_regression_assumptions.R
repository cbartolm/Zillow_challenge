######################################
# MS&E266: Mini-Project Milestone 3
# Bernardo Casares, Cristian Bartolome
######################################

library(Metrics)
library(dplyr)
library(GGally)
library(corrplot)
library(car)
library(gvlma)

rm(list = ls())
set.seed(1)

### Prediction on Train and Test-set

# Load Data
data_set = read.csv("../datasets_stanford/merged_train_2016.csv")
test_set = read.csv("../datasets_stanford/merged_test_2016.csv")

# Preprocessing functions
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
                                                "landtaxvaluedollarcnt", "taxvaluedollarcnt", "regionidcounty", 
                                                "fips", "censustractandblock")]
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

# Train-Dev Split
SplitData = function(data_set, trainin_value = 0.8){
  # Split into training and validation
  in.train = sample(nrow(data_set), size = as.integer(nrow(data_set)*trainin_value))
  training = data_set[in.train, ]
  validation = data_set[-in.train, ]
  split_data <- list("training"=training, "validation"=validation)
  return(split_data)
}

# Train-Dev data preparation

n_data = nrow(data_set)
data_set_total = rbind(data_set, test_set)
data_set_total = DataPreparation(data_set=data_set_total)
data_set = data_set_total[1:(n_data-24),]

split_data = SplitData(data_set=data_set)
training = split_data$training
validation = split_data$validation
testing = data_set_total[(n_data-24):nrow(data_set_total),]


data_set = training[c(1:100),c(1:20)]
M <- cor(data_set)
corrplot.mixed(M, tl.cex=0.5)

## Models ####
base_line = lm(logerror ~ . , data=data_set)

# Normality of Residuals

qqPlot(base_line, main="QQ Plot")
# distribution of studentized residuals

par(mfrow=c(2,2))  # draw 4 plots in same window
gvlma::gvlma(base_line)
#=>                     Value  p-value                   Decision
#=> Global Stat        15.801 0.003298 Assumptions NOT satisfied!
#=> Skewness            6.528 0.010621 Assumptions NOT satisfied!
#=> Kurtosis            1.661 0.197449    Assumptions acceptable.
#=> Link Function       2.329 0.126998    Assumptions acceptable.
#=> Heteroscedasticity  5.283 0.021530 Assumptions NOT satisfied!
plot(base_line)