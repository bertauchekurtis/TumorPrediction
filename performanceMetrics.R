# performance metrics
# kurtis bertauche
# 28 september 2022

library(ROCR)
library(glmnet)
library(e1071)
library(xgboost)
library(ranger)

test_data <- read.csv("data/clean/testing.csv")

# a frame to hold everything
performanceMetricFrame <- data.frame(accuracy = numeric(),
                                     sensitivity = numeric(),
                                     specificity = numeric(),
                                     false_positive_rate = numeric(),
                                     false_negative_rate = numeric(),
                                     positive_predictive_value = numeric(),
                                     negative_predictive_value = numeric(),
                                     area_under_roc_curve = numeric(),
                                     positive_likelihood_ratio = numeric(),
                                     negative_likelihood_ratio = numeric(),
                                     diagnostic_odds_ratio = numeric(),
                                     f1_score = numeric(),
                                     f2_score = numeric(),
                                     youden_index = numeric())


# a function to calc metrics
calcMetrics <- function(confusion_matrix, predictions, labels)
{
  accuracy <- confusion_matrix$overall[1]
  sensitivity <- confusion_matrix$byClass[1]
  specificity <- confusion_matrix$byClass[2]
  false_positive_rate <- (confusion_matrix$table[2]/(confusion_matrix$table[2] + confusion_matrix$table[1]))
  false_negative_rate <- (confusion_matrix$table[3]/(confusion_matrix$table[4]+confusion_matrix$table[3]))
  positive_predictive_value <- confusion_matrix$byClass[3]
  negative_predictive_value <- confusion_matrix$byClass[4]
  positive_likelihood_ratio <- sensitivity / false_positive_rate
  negative_likelihood_ratio <- false_negative_rate / specificity
  diagnostic_odds_ratio <- positive_likelihood_ratio / negative_likelihood_ratio
  f1_score <- (1 + 1^2) * ((sensitivity*positive_predictive_value)/(((1^2)*positive_predictive_value)+sensitivity))
  f2_score <- (1 + 2^2) * ((sensitivity*positive_predictive_value)/(((2^2)*positive_predictive_value)+sensitivity))
  youden_index <- sensitivity + specificity - 1
  pred <- prediction(predictions, labels)
  auc_perf <- performance(pred, measure = "auc")
  area_under_roc_curve <- auc_perf@y.values[[1]]
  
  returnFrame<- data.frame(accuracy = c(accuracy),
                           sensitivity = c(sensitivity),
                           specificity = c(specificity),
                           false_positive_rate = c(false_positive_rate),
                           false_negative_rate = c(false_negative_rate),
                           positive_predictive_value = c(positive_predictive_value),
                           negative_predictive_value = c(negative_predictive_value),
                           area_under_roc_curve = c(area_under_roc_curve),
                           positive_likelihood_ratio = c(positive_likelihood_ratio),
                           negative_likelihood_ratio = c(negative_likelihood_ratio),
                           diagnostic_odds_ratio = c(diagnostic_odds_ratio),
                           f1_score = c(f1_score),
                           f2_score = c(f2_score),
                           youden_index = c(youden_index))
  return(returnFrame)
  
}

# add lasso to data frame of metrics
load("models/lasso_model.RData")
load("results/lasso_confusion_matrix.RData")
test_data_glm_matrix <- model.matrix(Outcome ~ .,
                                     test_data)[,-1]
lasso_predictions <- predict(lasso_model, 
                             newx = test_data_glm_matrix, 
                             type = "response")
performanceMetricFrame <- rbind(performanceMetricFrame, 
                                calcMetrics(lasso_confusion_matrix,
                                            lasso_predictions,
                                            test_data$Outcome))
# add ridge
load("models/ridge_model.RData")
load("results/ridge_confusion_matrix.RData")
ridge_predictions <- predict(ridge_model,
                             newx = test_data_glm_matrix,
                             type = "response")
performanceMetricFrame <- rbind(performanceMetricFrame,
                                calcMetrics(ridge_confusion_matrix,
                                            ridge_predictions,
                                            test_data$Outcome))
# add elastic
load("models/elastic_net_model.RData")
load("results/elastic_net_confusion_matrix.RData")
elastic_predictions <- predict(elastic_net_model,
                               newx = test_data_glm_matrix,
                               type = "response")
performanceMetricFrame <- rbind(performanceMetricFrame,
                                calcMetrics(elastic_net_confusion_matrix,
                                            elastic_predictions,
                                            test_data$Outcome))
# add linear svm
load("models/best_linear_svm_model.RData")
load("results/linear_svm_confusion_matrix.RData")
linear_svm_predictions <- predict(best_linear_svm,
                                  test_data,
                                  probability = TRUE)
linear_svm_predictions <- attr(linear_svm_predictions, "probabilities")
linear_svm_predictions <- linear_svm_predictions[,2]
performanceMetricFrame <- rbind(performanceMetricFrame,
                                calcMetrics(linear_svm_confusion_matrix,
                                            linear_svm_predictions,
                                            test_data$Outcome))
# add radial svm
load("models/best_radial_svm_model.RData")
load("results/radial_svm_confusion_matrix.RData")
radial_svm_predictions <- predict(best_radial_svm,
                                  test_data,
                                  probability = TRUE)
radial_svm_predictions <- attr(radial_svm_predictions, "probabilities")
radial_svm_predictions <- radial_svm_predictions[,2]
performanceMetricFrame <- rbind(performanceMetricFrame,
                                calcMetrics(radial_svm_confusion_matrix,
                                            radial_svm_predictions,
                                            test_data$Outcome))

# add rf
load("models/random_forest_model.RData")
load("results/random_forest_confusion_matrix.RData")
rf_predictions <- predict(random_forest_model,
                          test_data)
rf_predictions <- rf_predictions$predictions[,2]
performanceMetricFrame <- rbind(performanceMetricFrame,
                                calcMetrics(random_forest_confusion_matrix,
                                            rf_predictions,
                                            test_data$Outcome))

# and add xgb
load("models/best_xgb_model_full.RData")
load("results/xgb_confusion_matrix.RData")
test_labels <- test_data$Outcome
test_data$Outcome <- NULL
xgb_test_data <- xgb.DMatrix(data.matrix(test_data), label = test_labels)
xgb_predictions <- predict(best_xgb_model_full,
                           xgb_test_data)
performanceMetricFrame <- rbind(performanceMetricFrame,
                                calcMetrics(xgb_confusion_matrix,
                                            xgb_predictions,
                                            test_labels))
rownames(performanceMetricFrame) <- c("lasso","ridge","elastic_net","linear_svm","radial_svm","random_forest","xgb")
performanceMetricFrame <- t(performanceMetricFrame)
save(performanceMetricFrame, file = "results/performanceMetricFrame.RData")
