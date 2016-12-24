library(ISLR)
names(Smarket)
summary(Smarket)
?Smarket
pairs(Smarket, col=Smarket$Direction)
cor(Smarket)
cor(Smarket[, -9]) #the only substantial correlation is between volume and year
plot(Volume ~ Year, data = Smarket)
boxplot(Volume ~ Year, data = Smarket)
# Logistic regression
fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
          data = Smarket, family = binomial)
summary(fit)
probs = predict(fit, type="response") 
probs[1:5]
contrasts(Smarket$Direction) # probabilities of the market going up
pred = ifelse(probs > 0.5, "Up", "Down")
#confusion matrix
table(pred, Smarket$Direction)
(145 + 507)/ 1250
mean(pred == Smarket$Direction)
# Make training and test set
train = Smarket$Year < 2005
fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
          data = Smarket, family = binomial, subset = train)
probs = predict(fit, newdata = Smarket[!train,], type="response") 
pred = ifelse(probs > 0.5, "Up", "Down")
Direction.2005 = Smarket$Direction[!train]
table(pred, Direction.2005)
mean(pred != Direction.2005)
#Fit smaller model
fit = glm(Direction ~ Lag1 + Lag2,
          data = Smarket, family = binomial, subset = train)
probs = predict(fit, newdata = Smarket[!train,], type="response") 
pred = ifelse(probs > 0.5, "Up", "Down")
table(pred, Direction.2005)
mean(pred == Direction.2005)
106/(76+106)
predict(fit, newdata = data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)), type = "response")
