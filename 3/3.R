library(MASS)
?Boston

Boston$chas <- as.factor(Boston$chas)
Boston$rad <- as.factor(Boston$rad)

fit.zn <- lm(crim ~ zn, Boston)
summary(fit.zn)

fit.indus <- lm(crim ~ indus, Boston)
summary(fit.indus)

fit.chas <- lm(crim ~ chas, Boston)
summary(fit.chas)

fit.nox <- lm(crim ~ nox, Boston)
summary(fit.nox)

fit.rm <- lm(crim ~ rm, Boston)
summary(fit.rm)

fit.age <- lm(crim ~ age, Boston)
summary(fit.age)

fit.dis <- lm(crim ~ dis, Boston)
summary(fit.dis)

fit.rad <- lm(crim ~ rad, Boston)
summary(fit.rad)

fit.tax <- lm(crim ~ tax, Boston)
summary(fit.tax)

fit.ptratio <- lm(crim ~ ptratio, Boston)
summary(fit.ptratio)

fit.black <- lm(crim ~ black, Boston)
summary(fit.black)

fit.lstat <- lm(crim ~ lstat, Boston)
summary(fit.lstat)

fit.medv <- lm(crim ~ medv, Boston)
summary(fit.medv)

plot(Boston$crim ~ Boston$tax)
abline(fit.tax)

plot(Boston$crim ~ Boston$rad)
abline(fit.rad)

plot(Boston$crim ~ Boston$black)
abline(fit.black)

plot(Boston$crim ~ Boston$lstat)
abline(fit.lstat)


fit <- lm(crim ~ ., Boston)
summary(fit)

x <- fit.zn$coefficients[2]
x <- c(x, fit.indus$coefficients[2])
x <- c(x, fit.chas$coefficients[2])
x <- c(x, fit.nox$coefficients[2])
x <- c(x, fit.rm$coefficients[2])
x <- c(x, fit.age$coefficients[2])
x <- c(x, fit.dis$coefficients[2])
x <- c(x, fit.rad$coefficients[2])
x <- c(x, fit.tax$coefficients[2])
x <- c(x, fit.ptratio$coefficients[2])
x <- c(x, fit.black$coefficients[2])
x <- c(x, fit.lstat$coefficients[2])
x <- c(x, fit.medv$coefficients[2])

x

plot(x, fit$coefficients[2:14], xlab = "Парная регрессия", ylab = "Множественная регрессия")
identify(x, fit$coefficients[2:14])

rm.sq <- Boston$rm * Boston$rm
rm.qub <- rm.sq * Boston$rm

fit.rm.sq <- lm(crim ~ rm + rm.sq + rm.qub, Boston)
summary(fit.rm.sq)


fits <- lapply(1:ncol(Boston), function(x) {
  if (!is.factor(Boston[, x])) {
    formula <- poly(Boston[, x], 3)
  } else {
    formula <- Boston[, x]
  }
  
  lm(crim ~ formula, Boston)
})

names(fits) <- colnames(Boston)

lapply(fits, summary)


