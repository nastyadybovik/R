install.packages("ISLR")
library(ISLR)
?Carseats

str(Carseats)

lm.sales <- lm(Sales ~ Price + Urban + US, Carseats)
summary(lm.sales)

lm.sales.wurban <- lm(Sales ~ Price + US, Carseats)
summary(lm.sales.wurban)

lm.sales.wurban$coefficients
confint(lm.sales.wurban)

plot(lm.sales.wurban)


set.seed(1)
x1 <- runif(100)
x2 <- 0.5*x1 + rnorm(100)/10
y <- 2 + 2*x1 + 0.3 *x2 + rnorm(100)

cor(x1, x2)
plot(x1, x2)

y.lm <- lm(y ~ x1 + x2)
summary(y.lm)

y.lm.x1 <- lm(y ~ x1)
summary(y.lm.x1)

y.lm.x2 <- lm(y ~ x2)
summary(y.lm.x2)

x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y, 6)

y.lm2 <- lm(y ~ x1 + x2)
summary(y.lm2)

y.lm2.x1 <- lm(y ~ x1)
summary(y.lm2.x1)

y.lm2.x2 <- lm(y ~ x2)
summary(y.lm2.x2)

par(mfrow= c(2,2))
plot(y.lm)
plot(y.lm2)

plot(y.lm.x1)
plot(y.lm2.x1)

plot(y.lm.x2)
plot(y.lm2.x2)
