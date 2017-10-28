library(data.table)

# useful commands
rm(list = ls()) # remove all variables
?fread          # get help
fread           # see source code

# Exploratory Data Analysis -------------------
DT <- fread('data/wages.csv')

str(DT)
summary(DT)
hist(DT$experience)
hist(DT$salary)


# Feature Engineering -------------------

# rename variables
setnames(DT, c("x", "y"))

DT[, x0 := 1]
for (k in 1:10) 
  set(DT, j=paste0("x", k), value=DT$x^k)

# remove variable
DT[, x := NULL]


# Training -------------------

# split data into a training and test set
set.seed(0)
indices <- DT[, sample(.N, 0.8*.N)]
train <- DT[ indices]
test  <- DT[-indices]

# predict salary from experience
fit <- lm(y~x0+x1-1, train)
summary(fit)

# Data Visualization -------------------
plot(train[, .(x1, y)], pch=16, col="red")
points(test[, .(x1, y)], pch=16, col="blue")
abline(fit$coefficients)

# Make predictions and evaluate fit -------------------
rmse <- function(y, yhat) {sqrt(mean((y-yhat)^2))}
mape <- function(y, yhat) {100*mean(abs(y-yhat)/y)}
preds <- predict(fit, test)
rmse(test$y, preds)
mape(test$y, preds)


# fit second degree polynomial
fit2 <- lm(y~x0+x1+x2-1, train)
summary(fit2)

# fit higher degree polynomial fit
fit3 <- lm(y~x1+x2+x3, train)
fit4 <- lm(y~x1+x2+x3+x4, train)


# f <- function(theta) {
#   x <- seq(0, 16, 0.1) 
#   Reduce(`+`, lapply(1:length(theta), function(k) theta[k] * x^(k-1)))    
# }


plotFit <- function(f, ...) {
  # plot dataset
  plot(train[, .(x1, y)], pch=16, col="red", ...)
  points(test[, .(x1, y)], pch=16, col="blue")
  
  # plot predictions
  theta <- f$coefficients
  x <- seq(0, 16, 0.1) 
  y <- Reduce(`+`, lapply(1:length(theta), function(k) theta[k] * x^(k-1)))  
  lines(x, y)
  grid()  
  
  # evaluate predictions
  preds <- predict(f, test)
  print(sprintf("Root Mean Squared Error (RMSE): %0.3f", rmse(test$salary, preds)))
  print(sprintf("Mean Absolute Precentage Error (MAPE): %0.3f%%", mape(test$salary, preds)))
}

plotFit(fit, xlim=c(1, 15), ylim=c(0,150000))
plotFit(fit2, xlim=c(1, 15), ylim=c(0,150000))
plotFit(fit3, xlim=c(1, 15), ylim=c(0,150000))
plotFit(fit4, xlim=c(1, 15), ylim=c(0,150000))


