# baseline models
# 19 september 2020
# kurtis bertauche

# libraries
library(glmnet)
library(caret)
set.seed(37)
# none yet

# open data
train_data <- read.csv("data/clean/training.csv")
test_data <- read.csv("data/clean/testing.csv")

# glm models require matrices
train_data_glm_matrix <- model.matrix(Outcome~., 
                                      train_data)[,-1]
test_data_glm_matrix <- model.matrix(Outcome ~ .,
                                     test_data)[,-1]
train_data_labels <- train_data$Outcome

# set up cv
foldid <- sample(rep(seq(5), length.out = nrow(train_data)))

# fit penalized lasso model (cv)
fit_lasso_cv <- cv.glmnet(x = train_data_glm_matrix,
                          y = train_data_labels, 
                          family = "binomial",
                          alpha = 1, 
                          nfolds = 5, 
                          foldid = foldid)
# select best model
lasso_model <- glmnet(x = train_data_glm_matrix,
                y = train_data_labels,
                family = "binomial",
                alpha = 1, 
                lambda = fit_lasso_cv$lambda.min)
# evaluate lasso
lasso_predictions <- predict(lasso_model, newx = test_data_glm_matrix, type = "response")
lasso_predictions <- ifelse(lasso_predictions > 0.5, 1, 0)
lasso_confusion_matrix <- confusionMatrix(data = factor(lasso_predictions), reference = factor(test_data$Outcome))
# save
save(lasso_model, file = "models/lasso_model.RData")
save(lasso_confusion_matrix, file = "results/lasso_confusion_matrix.RData")

# fit penalized ridge model (cv)
fit_ridge_cv <- cv.glmnet(x = train_data_glm_matrix,
                          y = train_data_labels,
                          family = "binomial",
                          alpha = 0,
                          nfolds = 5,
                          foldid = foldid)
# select best model
ridge_model <- glmnet(x = )

