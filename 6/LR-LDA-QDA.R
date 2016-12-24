library(ISLR)
library(MASS)
?Smarket
#linear discriminant analysis
train <- (Smarket$Year < 2005)
fit<- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
fit
pred <- predict(fit, Smarket[!train, ])
names(pred)
lda.class <- pred$class
pred$posterior[1:5, ]
Direction.2005 = Smarket$Direction[!train]
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)
#quadratic discriminant analysis
fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
fit
qda.class <- predict(fit, Smarket[!train, ])$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)
