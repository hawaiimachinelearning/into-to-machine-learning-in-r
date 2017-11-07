library(data.table)
wages <- fread('data/wages.csv')


# graphics functions ---------------------
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


# monotonic transformations ---------------------
plotWages(xlim=c(1, 30), ylim=c(1, 200000))
plotWagesFit(salary~poly(experience, 2), wages, col=2)
plotWagesFit(salary~ log(experience),    wages, col=3)
plotWagesFit(salary~sqrt(experience),    wages, col=4)
legend(0.5, 2.05e5, lwd=2, col=2:4, legend = c(
  "Polynomial (degree=2)", "Log-Transform", "Sqrt-Transform"))


# one-hot-encoding and feature interactions ---------------------
wages[, company_1 := (company==1)]
wages[, company_2 := (company==2)]
wages[, company_3 := (company==3)]

# make feature interactions
wages[, interaction_1 := sqrt(experience)*company_1]
wages[, interaction_2 := sqrt(experience)*company_2]
wages[, interaction_3 := sqrt(experience)*company_3]

# fit model with all features
fit_fe <- lm(salary~-1+company_1+company_2+company_3+
               interaction_1+interaction_2+interaction_3, wages)

# view preditions
plotWages(col=wages$company)
for (i in 1:3) {
  
  # initialize interactions
  tmp <- data.table(experience=seq(0,50, 0.01))
  tmp[, paste0("company_", 1:3) := F]  
  tmp[, paste0("interaction_", 1:3) := 0]
  
  # create specified interaction
  set(tmp, j=paste0("company_", i), value=T)
  set(tmp, j=paste0("interaction_", i), value=sqrt(tmp$experience))
  
  # add model predictions
  lines(tmp$experience, predict(fit_fe, tmp), col=i, lwd=2)  
}
legend(1, 1.4e5, legend = paste0("Company ", 1:3), pch=16, col=1:3)

