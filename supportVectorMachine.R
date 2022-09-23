# support vector regression
# kurtis bertauche
# 23 september 2022

# libraries
library(e1071)
set.seed(37)

# load data
train_data <- read.csv("data/clean/training.csv")
test_data <- read.csv("data/clean/testing.csv")

costs = c(0.01, 0.05, 0.1, 0.5, 1, 2, 5, 10, 20, 50)

svm_model <- NULL
accuracy <- 0

for(cost in costs)
{
  train_control <- trainControl(method = "cv", number = 5)
  set.seed(37)
  svm_model_tune <- train(factor(Outcome) ~ .,
                          data = train_data,
                          method = "svmLinear",
                          type = "C-svc",
                          trControl = train_control,
                          tuneGrid = data.frame(C=c(cost)))
  
  if(svm_model_tune$results$Accuracy > accuracy)
  {
    accuracy <- svm_model_tune$results$Accuracy
    svm_model <- svm_model_tune
    save(svm_model, file = "models/svm_model.RData")
    print("New Best Model! Accuracy:")
    print(accuracy)
    print("Cost:")
    print(cost)
  }
  else
  {
    print("Model Finished - no improvement - Cost:")
    print(cost)
  }
}

for(cost in costs)
{
  train_control <- trainControl(method = "cv", number = 5)
  set.seed(37)
  svm_model_tune <- train(factor(Outcome) ~ .,
                          data = train_data,
                          method = "svmRadial",
                          type = "C-svc",
                          trControl = train_control,
                          tuneGrid = data.frame(C=c(cost)))
  
  if(svm_model_tune$results$Accuracy > accuracy)
  {
    accuracy <- svm_model_tune$results$Accuracy
    svm_model <- svm_model_tune
    save(svm_model, file = "models/svm_model.RData")
    print("New Best Model! Accuracy:")
    print(accuracy)
    print("Cost:")
    print(cost)
  }
  else
  {
    print("Model Finished - no improvement - Cost:")
    print(cost)
  }
}

