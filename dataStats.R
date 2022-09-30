# data statistics
# kurtis bertauche
# 30 september 2022

train_data <- read.csv("data/clean/training.csv")
test_data <- read.csv("data/clean/testing.csv")

# TOTAL NUMBER OF SAMPLES
nrow(train_data) + nrow(test_data)

# TOTAL NUMBER OF TUMOR SAMPLES
length(which(train_data$Outcome == 1)) + length(which(test_data$Outcome == 1))

# TOTAL NUMBER OF NORMAL TISSUE SAMPLES
length(which(train_data$Outcome == 0)) + length(which(test_data$Outcome == 0))

# number of input variables
ncol(train_data) - 1

