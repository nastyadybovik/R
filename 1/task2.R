library(MASS)

View(Boston)

str(Boston)

plot(Boston$age)
plot(Boston$zn) 
plot(Boston$indus)

pairs(Boston)

plot(Boston$crim)
identify(Boston$crim)
plot(Boston$tax)
identify(Boston$tax)
plot(Boston$ptratio)
summary(Boston$crim)

sum(Boston$chas)

summary(Boston$ptratio)

index.row <- which.min(Boston$medv)
index.row
plot(Boston$medv)
print(Boston[index.row, ])

summary(Boston)

nrow(Boston[Boston$rm > 7, ])
sum(Boston$rm > 8)
Boston[Boston$rm > 8, ]
