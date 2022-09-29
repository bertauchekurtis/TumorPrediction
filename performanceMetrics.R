# performance metrics
# kurtis bertauche
# 28 september 2022

library(ROCR)
library(glmnet)

test_data <- read.csv("data/clean/testing.csv")

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

load("models/lasso_model.RData")
load("results/lasso_confusion_matrix.RData")
test_data_glm_matrix <- model.matrix(Outcome ~ .,
                                     test_data)[,-1]
lasso_predictions <- predict(lasso_model, 
                             newx = test_data_glm_matrix, 
                             type = "response")
hm <- calcMetrics(lasso_confusion_matrix,
            lasso_predictions,
            test_data$Outcome)
