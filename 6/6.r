library(ISLR)
library(MASS)
summary(Weekly)
?Weekly

train <- sample(1:nrow(Weekly), size = nrow(Weekly)*0.7)

direction.lag2.lda <- lda(Direction ~ Lag2, Weekly, subset = train)
summary(direction.lag2.lda)
direction.lag2.lda

probs.lag2 <- predict(direction.lag2.lda, newdata = Weekly[-train, ], type = "response")
names(probs.lag2)
lda.class <- probs.lag2$class
direction.test <- Weekly$Direction[-train]
table(lda.class, direction.test)
mean(lda.class == direction.test)



direction.lag2.qda <- qda(Direction ~ Lag2, Weekly, subset = train)
summary(direction.lag2.qda)
direction.lag2.qda

probs.lag2.qda <- predict(direction.lag2.qda, newdata = Weekly[-train, ], type = "response")
lda.class.qda <- probs.lag2.qda$class
table(lda.class.qda, direction.test)
mean(lda.class.qda == direction.test)


?Auto
auto <- Auto
auto$mpg01 <- ifelse(Auto$mpg > median(Auto$mpg), "1", "0")
auto$mpg01 <- as.factor(auto$mpg01)
pairs(auto, col = auto$mpg01)

summary(auto$year)
head(auto$year)

auto.train <- sample(1:nrow(auto), size = nrow(auto)*0.7)
auto.train

mpg01.lda <- lda(mpg01 ~ horsepower + acceleration, auto, subset = auto.train)
summary(mpg01.lda)
mpg01.lda

probs.mpg01.lda <- predict(mpg01.lda, newdata = auto[-auto.train, ])
mpg01.lda.class <- probs.mpg01.lda$class
mpg01.test <- auto$mpg01[-auto.train]
table(mpg01.lda.class, mpg01.test)
mean(mpg01.lda.class == mpg01.test)


mpg01.qda <- qda(mpg01 ~ horsepower + acceleration, auto, subset = auto.train)
summary(mpg01.qda)
mpg01.qda

probs.mpg01.qda <- predict(mpg01.qda, newdata = auto[-auto.train, ])
mpg01.qda.class <- probs.mpg01.qda$class
table(mpg01.qda.class, mpg01.test)
mean(mpg01.qda.class == mpg01.test)
