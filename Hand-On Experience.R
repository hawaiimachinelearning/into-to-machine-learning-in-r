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


# Feature Engineering ---------------------

#' 1) Load in the sensors.csv dataset.

#' 2) Create a function to plot temperature vs year.
#' Hint: modify the plotWages function

#' 3) Create a feature seasonal = sin(2*pi*year + pi/6). 

#' 4) Create a function to fit and plot the predictions from 
#' a model of the form temperature~seasonal+poly(year, 2). 
#' 
#' Hint: Create a function to do the sinusoidal 
#' transformation, then modify the plotWagesFit function
#' to pass this function in as a parameter.


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

#' 2) Plot the average validation error vs mtry.  

#' 3) What value of mtry produces the lowest average validation error?

  
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

#' 2) Plot the average validation error vs alph.

#' 3) What value of alpha produces the lowest average validation error?
