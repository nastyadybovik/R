###basic commands
x <- c(1, 3, 2, 5)
x
x = c(1, 6, 2)
x
x = 1:4
length(x)
str(x)
y = seq(from = 4, length = 3, by = 3)
?seq
y = seq(from = 4,  by = 3, length = 3)
z = rep("Yes", 10)
str(z)
y
x + y
x = 1:3
x + y
x/y
x^y
x[2]
x[2:3]
x[-2]
x[-c(1, 2)]
z = matrix(seq(1, 12), nrow = 4, ncol = 3)
z
z = matrix(seq(1, 12), nrow = 4, ncol = 3, byrow = TRUE)
z
z[3:4, 2:3]
z[ ,2:3]
z[ ,1]
z[, 1, drop=FALSE]
dim(z)
ls()
rm(y)
ls()
### Generating random data, graphics
x = runif(50)
y = rnorm(50)
set.seed(1204)
x = runif(50)
mean(x)
var(x)
sqrt(var(x))
sd(x)
plot(x, y)
plot(x, y, xlab="Random Uniform", ylab="Random Normal", main = "Plot of X vs Y", pch="*", col="blue")
?plot
par(mfrow = c(2,1))
plot(x, y)
hist(y)
par(mfrow = c(1,1))
### Reading in data
Auto = read.csv("Auto.csv")
getwd()
setwd("D:/BigData/EPAM-lectures/Labs")
getwd()
Auto = read.csv("Auto.csv")
fix(Auto)
names(Auto)
dim(Auto)
class(Auto)
str(Auto)
summary(Auto)
plot(Auto$cylinders, Auto$mpg)
plot(Auto$cyl, Auto$mpg)
attach(Auto)
search()
plot(cylinders, mpg)
cylinders = as.factor(cylinders)
plot(cylinders, mpg, xlab="Cylinders", ylab="Mpg", col="red")
hist(mpg)
hist(mpg, col = 2)
hist(mpg, col = 2, breaks = 15)
pairs(Auto, col = "brown")
pairs(~mpg + cylinders + acceleration + weight, Auto)
plot(horsepower, mpg)
identify(horsepower, mpg, name)


