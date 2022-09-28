# performance metrics
# kurtis bertauche
# 28 september 2022

performanceMetricFrame <- data.frame(accuracy = numeric(),
                                     sensitivity = numeric(),
                                     specificity = numeric(),
                                     false_positive_rate = numeric(),
                                     false_negative_rate = numeric(),
                                     positive_predictive_value = numeric(),
                                     negative_predictive_value = numeric(),
                                     area_under_roc_curve = numeric(),
                                     postivie_likelihood_ratio = numeric(),
                                     negative_likelihood_ratio = numeric(),
                                     diagnostic_odds_ratio = numeric(),
                                     f1_score = numeric(),
                                     f2_score = numeric(),
                                     youden_index = numeric())

calcMetrics <- function(confusion_matrix)
{
  
}