# Exploratiory Data Analysis ---------------------

#' 1) Load in the wages.csv dataset.
wages <- fread('data/wages.csv')

#' 2) Use plotWages and plotWagesFit to plot fitted 
#' polynomials from degree 1 to 25.

# plot experience vs salary
plotWages <- function(...) {
  plot(wages[, .(experience, salary)], pch=16, ...)  
  grid(col = rgb(0,0,0,0.2))
}

# plot fitted model 
plotWagesFit <- function(formula, DT, ...) {
  x <- seq(0,50, 0.01)
  fit <- lm(formula, DT)
  preds <- predict(fit, data.table(experience=x))
  lines(x, preds, lwd=2, ...)
}

plotWages()
for (i in 1:25) 
  plotWagesFit(salary~poly(experience, i), wages, col=i)


# Feature Engineering ---------------------

#' 1) Load in the sensors.csv dataset.
sensors <- fread('data/sensors.csv')

#' 2) Create a function to plot temperature vs year.
#' Hint: modify the plotWages function
plotSensors <- function(...) {
  plot(sensors[, .(year, temperature)], pch=16, ...)  
  grid(col = rgb(0,0,0,0.2))
}
plotSensors()

#' 3) Create a feature seasonal = sin(2*pi*year + pi/6). 
sinusoid <- function(year) sin(2*pi*year + pi/6)
sensors[, seasonal := sinusoid(year)]

#' 4) Create a function to fit and plot the predictions from 
#' a model of the form temperature~seasonal+poly(year, 2). 
#' 
#' Hint: Create a function to do the sinusoidal transformation,
#' then modify the plotWagesFit function to pass this function
#' in as a parameter.
plotSensorsFit <- function(formula, DT, sinusoid, ...) {
  x <- seq(0,10, 0.01)
  fit <- lm(formula, DT)
  preds <- predict(fit, data.table(year=x, seasonal=sinusoid(x)))
  lines(x, preds, lwd=2, ...)
}

plotSensorsFit(temperature~seasonal+poly(year, 2), sensors, sinusoid, col=3)


# Model Selection (hyperparameter tuning) ---------------------
#' 1) Use 10-fold cross-validation to optimize the  mtry parameter
#'  in the randomForest function.
library(randomForest)
library(caret)

rmse <- function(y_true, y_pred) {sqrt(mean((y_true-y_pred)^2))}

# load data
wages <- fread('data/wages.csv')

# one-hot-encoding and feature interactions 
wages[, company_1 := (company==1)]
wages[, company_2 := (company==2)]
wages[, company_3 := (company==3)]

# make feature interactions
wages[, interaction_1 := sqrt(experience)*company_1]
wages[, interaction_2 := sqrt(experience)*company_2]
wages[, interaction_3 := sqrt(experience)*company_3]

# create folds
set.seed(0)
n_folds <- 10
folds <- createFolds(wages$salary, n_folds)

# iterate through mtry in 1:8
cv_log <- data.table(mtry=1:8, error=0)
for (m in cv_log$mtry) {
  
  oof_error <- rep(NA, n_folds)
  for (k in 1:n_folds) {
    
    # split data
    inds <- folds[[k]]
    train <- wages[-inds]
    validation <- wages[inds]
    
    # fit model
    fit_cv <- randomForest(salary~., train, mtry=m)
    
    # evaluate fit
    preds <- predict(fit_cv, validation)
    oof_error[k] <- rmse(validation[, salary], preds)
  }
  
  # update log
  cv_log[mtry==m, error := mean(oof_error)]
}

#' 2) Plot the average validation error vs mtry.  
plot(cv_log, pch=16, ylab="out-of-fold error",
     main="Cross-Validation - randomForest")
lines(cv_log)
grid(col = rgb(0,0,0,0.2))

#' 3) What value of mtry produces the lowest average validation error?
cv_log[which.min(error), mtry]


# Model Selection (regularization) ---------------------
#' Use bagging to optimize the alpha parameter in the
#' glmnet function. Repeat the bagging procedure 10 times.
#' https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
library(glmnet)

rmse <- function(y_true, y_pred) {sqrt(mean((y_true-y_pred)^2))}

# load data
wages <- fread('data/wages.csv')

# one-hot-encoding and feature interactions 
wages[, company_1 := (company==1)]
wages[, company_2 := (company==2)]
wages[, company_3 := (company==3)]

# make feature interactions
wages[, interaction_1 := sqrt(experience)*company_1]
wages[, interaction_2 := sqrt(experience)*company_2]
wages[, interaction_3 := sqrt(experience)*company_3]

# feature matrix and target vector
features <- as.matrix(wages[, -"salary"])
target <- wages$salary

n_rep <- 10

# iterate through phase in seq(0, 1, 0.1)
bagging_log <- data.table(alpha=seq(0, 1, 0.1), error=0)
for (a in bagging_log$alpha) {
  
  oob_error <- rep(NA, n_rep)
  for (k in 1:n_rep) {
    
    # split data
    train_inds <- wages[, sample(.N, .N, replace=T)]
    validation_inds <- wages[, setdiff(.I, train_inds)]
    
    train_matrix <- features[train_inds, ]
    validation_matrix <- features[validation_inds, ]
    
    # fit model
    fit_bag <- glmnet(features, target, alpha=a)
    
    # evaluate fit
    preds <- predict(fit_bag, validation_matrix)
    oob_error[k] <- rmse(target[validation_inds], preds)
  }
  
  # update bagging log
  bagging_log[alpha==a, error := mean(oob_error)]
}


#' 2) Plot the average validation error vs alpha.
plot(bagging_log, pch=16, ylab="out-of-bag error",
     main="Bagging - GLMNET")
lines(bagging_log)
grid(col = rgb(0,0,0,0.2))

#' 3) What value of alpha produces the lowest average validation error?
bagging_log[which.min(error), alpha]
