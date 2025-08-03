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
# for an example of persp() we can also apply the function expand.grid()
Fact.1 <- seq(1, 15, by = 1)
Fact.2 <- seq(1, 15, by = 1)
Funct <- function(Fact.1, Fact.2)
	(10 - log(Fact.1, base = 10)) + 0.5*log(Fact.2, base = 10)
Answer <- outer(Fact.1, Fact.2, Funct)

persp(Fact.1, Fact.2, Answer)
persp(Fact.1, Fact.2, Answer, theta = 45, phi = 30, shade = 0.4, col = "green")

par(mfrow = c(1,3))
persp(Fact.1, Fact.2, Answer, theta = -45, shade = 0.4, col = "green")
persp(Fact.1, Fact.2, Answer, theta = 0, shade = 0.4, col = "green")
persp(Fact.1, Fact.2, Answer, theta = 45, shade = 0.4, col = "green")

contour(Fact.1, Fact.2, Answer)

par(mfrow = c(1,2))
persp(Fact.1, Fact.2, Answer, shade = 0.4, col = "green")
contour(Fact.1, Fact.2, Answer)

# example of addition of plots in existing plots
plot(iris$Petal.Length, iris$Petal.Width)
boxplot(iris$Petal.Width, at = 4, add = TRUE)
boxplot(iris$Petal.Length, horizontal = TRUE, at = 1.5, add = TRUE)

# 7: Plotting with the R-package lattice ---------------------------------------

library(lattice)

Table <- expand.grid(x = seq(1, 15, by = 1), y = seq(1, 15, by = 1))
Table$z <- (10 - log(Table$x, base = 10)) + 0.5*log(Table$y, base = 10)

wireframe(z ~ x * y, data = Table)
wireframe(z ~ x * y, data = Table, xlab = "factor 1", ylab = "factor 2",
		zlab = "answer", shade = TRUE, screen = list(z = -45, x = -90, y = 20))

data(iris)
xyplot(Petal.Length ~ Petal.Width | Species, data = iris)
bwplot(Petal.Length ~ Species, data = iris)
cloud(Petal.Length ~ Sepal.Length * Sepal.Width | Species, data = iris)

library(vegan)
data(dune.env)

cloud(Moisture ~ A1 * Manure | Use, data = dune.env, col = dune.env$Management)
bwplot(A1 ~ Use | Management, data = dune.env)

