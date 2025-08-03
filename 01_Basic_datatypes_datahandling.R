# Introduction to R
# Basic commands
# 
# Script based on a course of Dr. Miguel Alvarez (hold in the Graduate School of the
# University of Freiburg)
################################################################################

# 1: Basic commands ------------------------------------------------------------

# mathematic operators
1+2*3
(1+2)*3
2*5^2-10*5
4*sin(pi/2)
0/0

# logistic operators
5 > 4
5 < 4
5 == 4
5 != 4

# assignments
A <- 5
8 -> B
C = 9
a <- 9
A == a # R is case sensitive (A is not equal to a)

A/B
A+C

# objects: vectors
A <- vector(mode = "numeric", length = 10)
B <- c(1,2,3,4)
c(1:4)
C <- rep(3, times = 20)
D <- sample(c(1:100), 20, replace = TRUE)

# objects: matrices
matrix(ncol = 10, nrow = 5)
mat <- matrix(ncol = 10, nrow = 5)

A <- c(1:5)
B <- c(6:10)
rbind(A,B)
cbind(A,B)

# objects: data.frame
Table <- data.frame(Var.1 = A, Var.2 = B, Var.3 = "bla")

# short overview in the help functions
?data.frame

# 2: Reading and writing data from textfile 

tab <- read.table("dataset.txt", header=T, sep="\t")

colnames(tab) <- c("","","","")

write.table(tab, "dataset2.txt", sep = "\t")

# 3: Data handling -------------------------------------------------------------

require(raster)

img <- stack("F:/R_Course_Slovenia/1_General_Intro/1_0_general_introduction_to_R/petal_sepal_label.png")
plot(img)
plot(img, col=gray.colors(10, start = 0, end = 1, gamma = 2.2, alpha = NULL))

# general
data(iris)
str(iris)
summary(iris)

# accessing to the different variables
Table <- iris

Table[1,]
Table[,1]
Table$Sepal.Length
names(Table)
colnames(Table)
rownames(Table)

length(Table[1,])
length(Table[,1])

# overwriting data
Table$Petal.Length
Table$Petal.Length[Table$Petal.Length <= 1.3] <- 1
Table$Petal.Length[Table$Petal.Length > 1.3] <- 2

#use of subset
Table <- subset(iris, Species == "setosa")
Table02 <- split(iris, iris$Species)

# use of subset and %in%
vect.sel <- c("setosa", "virginica")
Table <- subset(iris, Species %in% vect.sel)

vect.sel <- sample(rownames(iris), 20, replace = FALSE)
Table <- subset(iris, rownames(iris) %in% vect.sel)

# sorting by columns
Table <- iris
Table[order(Table$Sepal.Length, decreasing = FALSE),]

