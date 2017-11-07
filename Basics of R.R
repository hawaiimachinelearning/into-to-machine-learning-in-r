# everything that exists is an object
x <- 1 + 1
class(x)
class(mean)

# everything that happens is a function call
`+`(1, 1)

x <- 1:3
y <- 4:6
cbind(x, y, add=x+y, subtract=x-y)
for (op in c(`+`, `-`)) print(op(x, y))

# useful commands 
?mean # get help with a function

rm(list = ls()) # deletes all variables in environment
gc() # free up RAM