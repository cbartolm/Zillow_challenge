# Load Data
data_set = read.csv("Data/merged_train_2016.csv")
test_set = read.csv("Data/merged_test_2016.csv")

# Preprocessing functions
source('~/Documents/git_hub/Zillow_challenge/clean_data_functions.R')

# Train-Dev data preparation
n_data = nrow(data_set)
data_set_total = rbind(data_set, test_set)
data_set_total = DataPreparation(data_set=data_set_total)
data_set = data_set_total[1:(n_data-24),]

split_data = SplitData(data_set=data_set)
training = split_data$training
validation = split_data$validation
testing = data_set_total[(n_data-24):nrow(data_set_total),]