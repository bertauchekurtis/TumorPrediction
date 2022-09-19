# baseline models
# 19 september 2020
# kurtis bertauche

library(glmnet)

train_data <- read.csv("data/clean/training.csv")

logistic_regression_model <- glm(Outcome ~ .,
                                 data = train_data,
                                 family = "binomial",
                                 maxit = 100)

save(logistic_regression_model, file = "models/logistic_regression_model.RData")

# matrix for glm
train_data_glm_matrix <- model.matrix(Outcome~., train_data)[,-1]
# train data label
train_data_labels <- train_data$Outcome

# set up cv
foldid <- sample(rep(seq(5), length.out = nrow(train_data)))

fit_lasso_cv <- cv.glmnet(train_data_glm_matrix,
                          train_data_labels, 
                          alpha = 1, 
                          nfolds = 5, 
                          foldid = foldid)



lasso <- glmnet(x, y, family = "binomial", alpha = 1, lambda = NULL)
