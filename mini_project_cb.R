#### MS&E 226, Small Data
#### Mini-Project I
#### Bernardo Casares, Cristian Bartolome 

rm(list = ls())

train = read.csv("../datasets_stanford/merged_train_2016.csv")

all(is.na(train))

# Data Preparation
train$hashottuborspa <- ifelse(train$hashottuborspa == 'true', 1, 0)
train$fireplaceflag <- ifelse(train$fireplaceflag == 'true', 1, 0)
train$taxdelinquencyflag <- ifelse(train$taxdelinquencyflag == 'Y', 1, 0)
train$propertycountylandusecode <- as.numeric(as.factor(train$propertycountylandusecode))
train$propertyzoningdesc <- as.numeric(as.factor(train$propertyzoningdesc))

train$transactiondate <- NULL
train$transactiondate <- NULL


# dtrain <- train[, !c('logerror', 'parcelid', 'transactiondate'), with=FALSE]


# train <- train[, !names(train) %in% c('parcelid', 'transactiondate')]


print('Building model ...')
lr_baseline <- lm(logerror ~ ., data=train);
summary(lr_baseline) # view the model