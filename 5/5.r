library(ISLR)
summary(Weekly)
?Weekly
pairs(Weekly, col=Weekly$Direction)

direction.glm <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, Weekly, family = binomial)
summary(direction.glm)

probs <- predict(direction.glm, type = "response")
head(probs)
contrasts(Weekly$Direction)

elements <- ifelse(probs > 0.5, "Up", "Down")
confusion.matrix <- table(elements, Weekly$Direction)
accuracy <- (confusion.matrix[1,1] + confusion.matrix[2,2]) / sum(confusion.matrix)
confusion.matrix

# Ошибка первого рода с вероятностью 0.3948577
# Ошибка второго рода с вероятностью 0.04407713

direction.lag2.glm <- glm(Direction ~ Lag2, Weekly[Weekly$Year < 2009, ], family = binomial)
summary(direction.lag2.glm)

probs.lag2 <- predict(direction.lag2.glm, type = "response")
head(probs.lag2)
contrasts(Weekly$Direction)

elements.lag2 <- ifelse(probs.lag2 > 0.5, "Up", "Down")
confusion.matrix.lag2 <- table(elements.lag2, Weekly$Direction[Weekly$Year < 2009])
accuracy.lag2 <- (confusion.matrix.lag2[1,1] + confusion.matrix.lag2[2,2]) / sum(confusion.matrix.lag2)
confusion.matrix.lag2

# Ошибка первого рода с вероятностью 0.4243655
# Ошибка второго рода с вероятностью 0.02030457

probs.lag2.new <- predict(direction.lag2.glm, newdata = Weekly[Weekly$Year > 2008, ], 
                          type = "response")
head(probs.lag2.new)
elements.lag2.new <- ifelse(probs.lag2.new > 0.5, "Up", "Down")
confusion.matrix.lag2.new <- table(elements.lag2.new, Weekly$Direction[Weekly$Year > 2008])
accuracy.lag2.new <- (confusion.matrix.lag2.new[1,1] + confusion.matrix.lag2.new[2,2]) / 
  sum(confusion.matrix.lag2.new)
confusion.matrix.lag2.new
accuracy.lag2.new

# Ошибка первого рода с вероятностью 0.3269231
# Ошибка второго рода с вероятностью 0.04807692



?Auto
auto <- Auto
auto$mpg01 <- ifelse(Auto$mpg > median(Auto$mpg), "1", "0")
auto$mpg01 <- as.factor(auto$mpg01)
pairs(auto, col = auto$mpg01)

train.index <- sample(1:nrow(auto), size = nrow(auto)*0.7)

auto.train <- auto[train.index, ]
auto.test <- auto[(train.index.end+1):nrow(auto), ]

mpg01.glm <- glm(mpg01 ~ horsepower, auto.train, family = binomial)
summary(mpg01.glm)

probs.mpg01 <- predict(mpg01.glm, newdata = auto[-train.index, ], type = "response")
head(probs.mpg01)
contrasts(auto$mpg01)

elements.mpg01 <- ifelse(probs.mpg01 > 0.5, "1", "0")
confusion.matrix.mpg01 <- table(elements.mpg01, auto$mpg01[-train.index])
accuracy.mpg01 <- (confusion.matrix.mpg01[1,1] + confusion.matrix.mpg01[2,2]) / 
  sum(confusion.matrix.mpg01)
confusion.matrix.mpg01

# Ошибка первого рода с вероятностью 0
# Ошибка второго рода с вероятностью 0.2288136