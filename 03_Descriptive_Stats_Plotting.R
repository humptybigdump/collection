# TODO:   Exercises for the first day of the R-course
# 
# Author: Dr. Miguel Alvarez
################################################################################

# DAY 2 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 5: Descriptive statistic -----------------------------------------------------

data(iris)
summary(iris)
mean(iris$Sepal.Length)
median(iris$Sepal.Length)
var(iris$Sepal.Length)
sqrt(var(iris$Sepal.Length)) # standard deviation
sqrt(var(iris$Sepal.Length))/sqrt(length(iris$Sepal.Length)) # standard error
sqrt(var(iris$Sepal.Length))/abs(mean(iris$Sepal.Length)) # coefficient of
														  # variation
# Explanation of attach() and detach()
attach(iris)
Sepal.Length
Petal.Length
detach(iris)
Sepal.Length
Petal.Length

# applying functions to columns and rows
Table <- iris
mean(Table)
mean(Table[,-5])

Table02 <- as.matrix(Table[,-5])
mean(Table02)
apply(Table02, 1, mean)
apply(Table02, 2, mean)

# applying sweep to standardize the data (substract mean and divide by standard deviation) 
iris.mean <- apply(Table02, 2, mean)
iris.stdev <- sqrt(apply(Table02, 2, var))
iris.trans <- sweep(Table02, 2, iris.mean, "-")
iris.trans <- sweep(iris.trans, 2, iris.stdev, "/")

# only to check
round(apply(iris.trans, 2, mean), digits = 3)
apply(iris.trans, 2, var)

# 6: Basics on plotting -------------------------------------------------------- 

data(iris)

plot(iris$Sepal.Length, iris$Petal.Length)
plot(iris$Sepal.Length, iris$Petal.Length, col = iris$Species)
points(4.5, 6, col = "green", pch = 16)
text(6.5, 2, labels = "this is an example")

indep.var <- seq(1, 22, by = 3)
dep.var <- 1 + 0.5*log(indep.var, base = 10)
plot(indep.var, dep.var)
lines(indep.var, dep.var, col = "green")
curve(1 + 0.5*log(x, base = 10), col = "orange", add = TRUE)

# high-level graphic functions
pairs(iris)
# pairs(iris, col = iris$Species)
hist(iris$Sepal.Length)
hist(iris$Petal.Length)
boxplot(Sepal.Length ~ Species, data = iris)
boxplot(Sepal.Width ~ Species, data = iris)
plot(iris$Sepal.Length, iris$Petal.Length)

