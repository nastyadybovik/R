Y = f(x) + e
среднее значение e равно 0
e не зависит от x
e обозначение ошибки
Парная линейная регрессия
y = b0 + b1*x + e
y^ = b0^ + b1^*x
ei = yi - yi^

  

library(MASS)
### Simple linear regression
names(Boston)
?Boston
plot(medv ~ lstat, Boston)
fit = lm(medv ~ lstat, data = Boston)
fit
summary(fit)
names(fit)
fit$coefficients
abline(fit)
abline(fit, lwd = 2)
abline(fit, lwd = 2, col="red")
confint(fit)
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1, 1))
plot(predict(fit), residuals(fit), xlab = "Predicted Values", ylab = "Residuals")
abline(0,0)
predict(fit, new = data.frame(lstat = c(25, 15)))


#true linear data 
x = rnorm(100, mean = 30, sd = 20)
eps = rnorm(100, mean = 0, sd = 10)
y = 2 + 3*x + eps
par(mfrow = c(1,1))
plot(y ~ x)

sample_fit = function(i){
  ind = sample(1:100, 10)
  fit = lm(y[ind] ~ x[ind])
  abline(fit, lwd = 1, col = "blue")
  fit$coefficients
}

sapply(1:10, sample_fit)
abline(2, 3, col = "red", lwd = 3)

#nonlinear data
x_n = runif(100, min = 0, max = 2 * pi)
eps_n = rnorm(100, mean = 0, sd = 0.5)
y_n = sin(x_n) + eps_n
par(mfrow = c(1,1))
plot(y_n ~ x_n)

sample_fit = function(i){
  ind = sample(1:100, 10)
  fit = lm(y_n[ind] ~ x_n[ind])
  abline(fit, lwd = 1, col = "blue")
  fit$coefficients
}

sapply(1:10, sample_fit)

fit_l = lm(y ~ x)
summary(fit_l)

fit_n = lm(y_n ~ x_n)
summary(fit_n)

par(mfrow = c(2,2))
plot(fit_l)
plot(fit_n)








