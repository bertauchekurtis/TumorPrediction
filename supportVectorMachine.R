# support vector regression
# kurtis bertauche
# 23 september 2022

# libraries
library(e1071)
library(caret)
set.seed(37)

# load data
train_data <- read.csv("data/clean/training.csv")
test_data <- read.csv("data/clean/testing.csv")

svm_model <- NULL

train_control <- trainControl(method = "cv", number = 5)

# LINEAR SVM
linear_svm <- train(as.factor(Outcome) ~.,
                    data = train_data,
                    method = "svmLinear",
                    trControl = train_control,
                    tuneGrid = expand.grid(C = seq(0.01, 2, length = 20)))
plot(linear_svm)

best_linear_svm <- svm(as.factor(Outcome) ~ .,
                         data = train_data,
                         kernel = "linear",
                         cost = linear_svm$bestTune)

svm_model <- best_linear_svm

# RADIAL KERNEL
raidal_svm <- train(as.factor(Outcome) ~.,
                    data = train_data,
                    method = "svmRadial",
                    trControl = train_control,
                    tuneLength = 20)

best_radial_svm <- svm(as.factor(Outcome) ~ .,
                         data = train_data,
                         kernel = "radial",
                         cost = raidal_svm$bestTune[2],
                         gamma = raidal_svm$bestTune[1])

# POLYNOMIAL KERNEL
poly_svm <- train(as.factor(Outcome) ~.,
                  data = train_data,
                  method = "svmPoly",
                  trControl = train_control,
                  tuneLength = 20)
best_poly_svm <- svm(as.factor(Outcome) ~ .,
                     data = train_data,
                     kernel = "polynomial",
                     cost = )
