library(data.table)
wages <- fread('data/wages.csv')

# view a summary and structure of the data ---------------------
wages
str(wages)
summary(wages)


# polynomial regression ---------------------

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

# linear model
plotWages()
plotWagesFit(salary~experience, wages)
legend(1, 1.4e5, legend="Linear Regression", lwd=2)

# second degree model
plotWages()
plotWagesFit(salary~poly(experience, 2), wages)
legend(1, 1.4e5, legend="Polynomial Regression", lwd=2)

# explore companies ---------------------
plotWages(col=wages$company)
legend(1, 1.4e5, legend=paste0("Company ", 1:3), pch=16, col=1:3)

# fit a polynomial to each company
plotWagesFit(salary~poly(experience, 2), wages[company==1], col=1)
plotWagesFit(salary~poly(experience, 2), wages[company==2], col=2)
plotWagesFit(salary~poly(experience, 2), wages[company==3], col=3)
