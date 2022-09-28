# xgb.R
# kurtis bertauche
# 26 september 2022

library(xgboost)
library(caret)
set.seed(37)

# load data
train_data <- read.csv("data/clean/training.csv")
test_data <- read.csv("data/clean/testing.csv")

train_labels <- train_data$Outcome
train_data$Outcome <- NULL

xgb_train_data <- xgb.DMatrix(data.matrix(train_data), label = train_labels)

test_labels <- test_data$Outcome
test_data$Outcome <- NULL

xgb_test_data <- xgb.DMatrix(data.matrix(test_data), label = test_labels)

# a matrix to hold hyperparameter combinations
matrixToTry <- matrix(,nrow=0,ncol=6)
for (gamma in c(0, 0.1, 0.2, 0.3, 0.4))
{
  for (child_weight in c(1,2,3,4,5,6))
  {
    for (col_subsample in c(0.6, 0.7, 0.8, 0.9))
    {
      for (max_depth in c(6, 7, 8, 9, 10, 11))
      {
        for (subsample in c(0.6, 0.7, 0.8, 0.9))
        {
          for (eta in c(0.01, 0.05, 0.08, 0.1))
          {
            matrixToTry <- rbind(matrixToTry,
                                 c(gamma,child_weight,
                                   max_depth, subsample, col_subsample,
                                   eta))
          }
        }
      }
    }
  }
}
row = 4
set.seed(37)
model <- xgb.cv(booster = "gbtree",
                objective = "binary:logistic",
                gamma = matrixToTry[row, 1],
                child_weight = matrixToTry[row, 2],
                max_depth = matrixToTry[row, 3],
                subsample = matrixToTry[row, 4],
                col_subsample = matrixToTry[row, 5],
                eta = matrixToTry[row, 6],
                nrounds = 10000,
                nthreads = 28,
                nfold = 5,
                print_every_n = 2500,
                early_stopping_rounds = 2,
                data = xgb_train_data,
                eval_metric = "logloss"
                )

best_xgb_model <- NULL
best_log_loss <- 999

for (row in 1:nrow(matrixToTry))
{
  set.seed(37)
  model <- xgb.cv(booster = "gbtree",
                  objective = "binary:logistic",
                  gamma = matrixToTry[row, 1],
                  child_weight = matrixToTry[row, 2],
                  max_depth = matrixToTry[row, 3],
                  subsample = matrixToTry[row, 4],
                  col_subsample = matrixToTry[row, 5],
                  eta = matrixToTry[row, 6],
                  nrounds = 10000,
                  nthreads = 28,
                  nfold = 5,
                  print_every_n = 2500,
                  early_stopping_rounds = 2,
                  data = xgb_train_data,
                  eval_metric = "logloss"
  )
  log_loss <- model$evaluation_log[model$best_iteration]$test_logloss_mean
  if(log_loss < best_log_loss)
  {
    best_xgb_model <- model
    best_log_loss <- log_loss
    print("NEW BEST MODEL FOUND!")
    print("ROW:")
    print(row)
  }
  else
  {
    print("MODEL DID NOT IMPROVE")
    print("ROW:")
    print(row)
  }
}

save(best_xgb_model, file = "models/best_xgb_model.RData")

# build model with full train set
set.seed(37)
best_xgb_model_full <- xgboost(booster = "gbtree",
                               objective = "binary:logistic",
                               gamma = best_xgb_model$params$gamma,
                               child_weight = best_xgb_model$params$child_weight,
                               max_depth = best_xgb_model$params$max_depth,
                               subsample = best_xgb_model$params$subsample,
                               col_subsample = best_xgb_model$params$col_subsample,
                               eta = best_xgb_model$params$eta,
                               nrounds = 10000,
                               nthreads = 28,
                               data = xgb_train_data,
                               eval_metric = "logloss",
                               early_stopping_rounds = 2)

xgb_predictions <- predict(best_xgb_model_full,
                           xgb_test_data)
xgb_predictions <- ifelse(xgb_predictions > 0.5,
                          1,
                          0)
xgb_confusion_matrix <- confusionMatrix(data = factor(xgb_predictions),
                                        reference = factor(test_labels))

save(best_xgb_model_full, file = "models/best_xgb_model_full.RData")
save(xgb_confusion_matrix, file = "results/xgb_confusion_matrix.RData")
