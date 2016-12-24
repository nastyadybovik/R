library(MASS)
?Boston
fit = lm(medv ~ lstat + age, data = Boston)
summary(fit)
fit = lm(medv ~ ., data = Boston)
summary(fit)
fit1 = lm(medv ~ .- age, data = Boston) 
summary(fit1)

# qualitative predictors
library(ISLR)
?Carseats
str(Carseats)
fit = lm(Sales ~ ., data = Carseats)
summary(fit)
contrasts(Carseats$ShelveLoc)

# interaction terms
fit = lm(medv ~ lstat + age + lstat:age, data = Boston)
summary(fit)
fit = lm(medv ~ lstat*age, data = Boston)
summary(fit)

# nonlinear transformation of the predictors
fit = lm(medv ~ lstat, data = Boston)
par(mfrow = c(2,2))
plot(fit)
fit = lm(medv ~ lstat + I(lstat^2), data = Boston)
summary(fit)
par(mfrow = c(2,2))
plot(fit)
fit = lm(medv ~ poly(lstat, 5), data = Boston)
summary(fit)
fit = lm(medv ~ log(rm), data = Boston)
summary(fit)

# checking for collinearity
library(car)
fit = lm(medv ~ ., data = Boston)
vif(fit)

# correlated errors
set.seed(376)
x = rnorm(100, 1, 3)
eps = rnorm(100, 0, 5)
y = -2 + 3*x + eps
par(mfrow = c(1,1))
plot(y ~ x)
fit = lm(y ~ x)
abline(fit, col = "red")
par(mfrow= c(2,2))
plot(fit)
cor_eps = rep(0, 100)
cor_eps[1] = rnorm(1,0,5)
for(i in 2:100){
  cor_eps[i] = 0.8*cor_eps[i-1] + 0.2*rnorm(1,0,5)
}
par(mfrow = c(2,1))
plot(1:100, eps, type = "b")
abline(a = 0, b = 0, col = "grey")
plot(1:100, cor_eps, type = "b")
abline(a = 0, b = 0, col = "grey")
y = -2 + 3*x + cor_eps
par(mfrow = c(1,1))
plot(y ~ x)
cor_fit = lm(y ~ x)
abline(cor_fit, col = "red")
par(mfrow= c(2,2))
plot(fit_cor)
summary(fit)
summary(cor_fit)
confint(fit, "x")
confint(cor_fit, "x")

# non-constant variance of error terms
het_eps = rep(0, 100)
for(i in 1:100){
  het_eps[i] = rnorm(1,0,5*0.01*i)
}
x = sort(x)
y = -2 + 3*x + het_eps
par(mfrow = c(1,1))
plot(y ~ x)
het_fit = lm(y ~ x)
abline(het_fit, col = "red")
par(mfrow= c(2,2)) 
plot(het_fit)