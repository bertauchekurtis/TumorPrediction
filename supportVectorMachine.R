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

train_control <- trainControl(method = "cv", number = 5)

# LINEAR SVM
linear_svm <- train(as.factor(Outcome) ~.,
                    data = train_data,
                    method = "svmLinear",
                    trControl = train_control,
                    tuneGrid = expand.grid(C = seq(0.01, 2, length = 20)))

best_linear_svm <- svm(as.factor(Outcome) ~ .,
                         data = train_data,
                         kernel = "linear",
                         cost = linear_svm$bestTune)

linear_svm_predictions <- predict(best_linear_svm,
                                  test_data)
linear_svm_confusion_matrix <- confusionMatrix(data = factor(linear_svm_predictions),
                                               reference = factor(test_data$Outcome),
                                               positive = "1")
save(best_linear_svm, file = "models/best_linear_svm_model.RData")
save(linear_svm_confusion_matrix, file = "results/linear_svm_confusion_matrix.RData")

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

radial_svm_predictions <- predict(best_radial_svm,
                                  test_data)
radial_svm_confusion_matrix <- confusionMatrix(data = factor(radial_svm_predictions),
                                               reference = factor(test_data$Outcome),
                                               positive = "1")
save(best_radial_svm, file = "models/best_radial_svm_model.RData")
save(radial_svm_confusion_matrix, file = "results/radial_svm_confusion_matrix.RData")
