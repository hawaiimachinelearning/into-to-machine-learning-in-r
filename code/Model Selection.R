library(data.table)
library(caret)

wages <- fread('data/wages.csv')
rmse <- function(y_true, y_pred) {sqrt(mean((y_true-y_pred)^2))}


# using a single validation set ---------------------
inds <- wages[, sample(.N, 0.2*.N)]
train <- wages[-inds] 
validation <- wages[inds] 

fit_single <- lm(salary~experience, train)
validation_preds <- predict(fit_single, validation)
sprintf("Validation RMSe: %0.3f", rmse(validation$salary, validation_preds))


# cross-validation ---------------------

# create folds
set.seed(1)
n_folds <- 10
folds <- createFolds(wages$salary, n_folds)

# iterate through degree in 1:10
cv_log <- data.table(degree=1:10, error=0)
for (d in cv_log$degree) {
  
  oof_error <- rep(NA, n_folds)
  for (k in 1:n_folds) {
    
    # split data
    inds <- folds[[k]]
    train <- wages[-inds]
    validation <- wages[inds]
    
    # fit model
    fit_cv <- lm(salary~poly(experience, d), train)
    
    # evaluate fit
    preds <- predict(fit_cv, validation)
    oof_error[k] <- rmse(validation$salary, preds)
  }
  
  # update log
  cv_log[degree==d, error := mean(oof_error)]
}

plot(cv_log, pch=16, ylab="out-of-fold error",
     main="Cross-Validation - Polynomial Regression")
lines(cv_log)
grid(col = rgb(0,0,0,0.2))

# bagging ---------------------

# repeat 10 times
n_rep <- 10
bagging_log <- data.table(degree=1:10, error=0)
for (d in bagging_log$degree) {
  
  oob_error <- rep(NA, n_folds)
  for (k in 1:n_rep) {
    
    # split data
    set.seed(k)
    train_inds <- wages[, sample(.N, .N, replace=T)]
    validation_inds <- wages[, setdiff(.I, train_inds)]
    
    train <- wages[train_inds]
    validation <- wages[validation_inds]
    
    # fit model
    fit_bag <- lm(salary~poly(experience, d), train)
    
    # evaluate fit
    preds <- predict(fit_bag, validation)
    oob_error[k] <- rmse(validation$salary, preds)
  }
  
  # update log
  bagging_log[degree==d, error := mean(oob_error)]
}

plot(bagging_log, pch=16, ylab="out-of-bag error",
     main="Bagging - Polynomial Regression")
lines(bagging_log)
grid(col = rgb(0,0,0,0.2))

