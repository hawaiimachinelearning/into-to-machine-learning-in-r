library(data.table)

# define company sizes
n_emp_A <- 30
n_emp_B <- 50
n_emp_C <- 100

# generate experience (uniformly distibuted) and company
set.seed(0)
DT <- data.table(
  company = c(
    rep(1L, n_emp_A),
    rep(2L, n_emp_B),
    rep(3L, n_emp_C)
  ),  
  experience = c(
    runif(n_emp_A, 1, 12),
    runif(n_emp_B, 1, 12),
    runif(n_emp_C, 1, 12)
  )
)

# generate salary (sqrt(experience) + gaussian noise)
getsalary <- function(x, y_min=40000, y_max=125000) {
  tmp <- sqrt(x)                                  # sqrt transformation
  tmp <- (tmp - min(tmp)) / (max(tmp) - min(tmp)) # standardize
  tmp <- (y_max - y_min) * tmp + y_min            # adjust range
  tmp + rnorm(length(tmp), sd = 7500)             # add noise
}

DT[company==1L, salary := getsalary(experience, 40000, 100000)]
DT[company==2L, salary := getsalary(experience, 60000, 80000)]
DT[company==3L, salary := getsalary(experience, 30000, 120000)]

# format data and save
DT[, experience := round(experience, 1)] # round to the tenth
DT[, salary := round(salary/5)*5]        # round to the nearest $5
DT <- DT[sample(.N)]                     # randomize order

fwrite(DT, "data/wages.csv")