library(MASS)
auto <- read.csv("Auto.csv", header = T)

View(auto)

mpg.hp.ml <- lm(mpg ~ horsepower, auto)
summary(mpg.hp.ml)

cor(auto$mpg, auto$horsepower)

predict(mpg.hp.ml, data.frame(horsepower = c(98)))

plot(auto$horsepower, auto$mpg)
abline(mpg.hp.ml, lwd = 3, col = 3)
par(mfrow = c(2,2))
plot(mpg.hp.ml)

set.seed(100)
x <- rnorm(100)
eps <- rnorm(100, 0, 0.25)

y <- -1 + 0.5*x + eps
y

par(mfrow = c(1, 1))
plot(y ~ x)

xy.lm <- lm(y ~ x)
summary(xy.lm)
abline(xy.lm, col = 4)
abline(-1, 0.5, col = 3, lwd = 2)
legend(-2.3, 0.5, legend = c("points", "linear model", "real"), col = c("black", 4, 3), lwd = 1, 
       lty = c(0, 1, 1), pch = c(1))


