# randomForest.R
# kurtis bertauche
# 23 september 2022

# libraries
library(ranger)
set.seed(37)

# load data
train_data <- read.csv("data/clean/training.csv")
test_data <- read.csv("data/clean/testing.csv")

# hold all the hyperparameters we wnat to try
hyperParameter_matrix <- matrix(,nrow=0,ncol=2)

random_forest_model <- NULL
random_forest_prediction_error <- 1

for(numTrees in c(100, 200, 300, 400, 500, 600, 750, 1000, 2000, 3000, 5000, 10000))
{
  for(mtry in c(100, 250, 500, 1000, 2000, 3000, 3500, 3600, 3700, 3800, 3900, 4000, 4200, 4400))
  {
    hyperParameter_matrix <- rbind(hyperParameter_matrix, c(numTrees, mtry))
  }
}

# tune the model
for(row in 1:nrow(hyperParameter_matrix))
{
  set.seed(37)
  rfModel <- ranger(formula = Outcome ~.,
                    data = train_data,
                    num.trees = hyperParameter_matrix[row,1],
                    mtry = hyperParameter_matrix[row,2],
                    min.node.size = 5,
                    num.threads = 24,
                    classification = TRUE)
  if(rfModel$prediction.error < random_forest_prediction_error)
  {
    random_forest_prediction_error <- rfModel$prediction.error
    random_forest_model <- rfModel
    save(random_forest_model, file = "models/random_forest_model.RData")
    print("New Best Model Found. Prediction Error:")
    print(random_forest_prediction_error)
    print("Num Trees:")
    print(hyperParameter_matrix[row,1])
    print("MTRY:")
    print(hyperParameter_matrix[row,2])
  }
  rm(rfModel)
}

# continue tuning based on initial observations
hyperParameter_matrix <- matrix(,nrow=0,ncol=2)
random_forest_model <- NULL
random_forest_prediction_error <- 1 # why restart? there are simpler options that might have the same results as the already previous model, and we prefer a simpler model

for(numTrees in c(100, 200, 300))
{
  for(mtry in c(3000, 3050, 3100, 3150, 3200, 3250, 3300, 3350, 3400, 3450, 3500))
  {
    hyperParameter_matrix <- rbind(hyperParameter_matrix, c(numTrees, mtry))
  }
}

for(row in 1:nrow(hyperParameter_matrix))
{
  set.seed(37)
  rfModel <- ranger(formula = Outcome ~.,
                    data = train_data,
                    num.trees = hyperParameter_matrix[row,1],
                    mtry = hyperParameter_matrix[row,2],
                    min.node.size = 5,
                    num.threads = 24,
                    classification = TRUE)
  if(rfModel$prediction.error < random_forest_prediction_error)
  {
    random_forest_prediction_error <- rfModel$prediction.error
    random_forest_model <- rfModel
    save(random_forest_model, file = "models/random_forest_model.RData")
    print("New Best Model Found. Prediction Error:")
    print(random_forest_prediction_error)
    print("Num Trees:")
    print(hyperParameter_matrix[row,1])
    print("MTRY:")
    print(hyperParameter_matrix[row,2])
  }
  rm(rfModel)
}

# evaluate best model
random_forest_predictions <- predict(random_forest_model,
                                     test_data)
random_forest_predictions <- random_forest_predictions$predictions
random_forest_confusion_matrix <- confusionMatrix(data = factor(random_forest_predictions),
                                                  reference = factor(test_data$Outcome),
                                                  positive = "1")

save(random_forest_confusion_matrix, file = "results/random_forest_confusion_matrix.RData")