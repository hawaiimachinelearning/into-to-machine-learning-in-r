library(data.table)

set.seed(0)
x <- seq(0, 5, 0.01)
y <- 20*sin(2*pi*x + pi/6) + 10*rnorm(length(x)) + 100*sin(pi*x/5)

plot(sensors, type="l")

sensors <- data.table(year=x, temperature=round(y, 5))
fwrite(sensors, "data/sensors.csv")