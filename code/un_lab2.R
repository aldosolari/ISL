library(MASS)
library(ISLR2)

head(Boston)

lm.fit <- lm(medv ~ lstat)

lm.fit <- lm(medv ~ lstat, data = Boston)
attach(Boston)
lm.fit <- lm(medv ~ lstat)

lm.fit
summary(lm.fit)

names(lm.fit)
coef(lm.fit)

confint(lm.fit)

predict(lm.fit, data.frame(lstat = (c(5, 10, 15))),
    interval = "confidence")
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))),
    interval = "prediction")

plot(lstat, medv)
abline(lm.fit)

plot(lstat, medv)
abline(lm.fit, lwd = 3)
abline(lm.fit, lwd = 3, col = "red")
plot(lstat, medv, col = "red")
plot(lstat, medv, pch = 20)
plot(lstat, medv, pch = "+")
plot(1:20, 1:20, pch = 1:20)

lm.fit2 <- lm(medv ~ lstat + I(lstat^2), data = Boston)
summary(lm.fit2)

lm.fit <- lm(medv ~ lstat, data = Boston)
anova(lm.fit, lm.fit2)

par(mfrow = c(2, 2))
plot(lm.fit2)

lm.fit5 <- lm(medv ~ poly(lstat, 5), data = Boston)
summary(lm.fit5)

summary(lm(medv ~ log(rm), data = Boston))

library(ISLR2)
set.seed(1)
train <- sample(392, 196)

lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)

attach(Auto)
mean((mpg - predict(lm.fit, Auto))[-train]^2)

lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, 
    subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, 
    subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

set.seed(2)
train <- sample(392, 196)
lm.fit <- lm(mpg ~ horsepower, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, 
    subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, 
    subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

glm.fit <- glm(mpg ~ horsepower, data = Auto)
coef(glm.fit)

lm.fit <- lm(mpg ~ horsepower, data = Auto)
coef(lm.fit)

library(boot)
glm.fit <- glm(mpg ~ horsepower, data = Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta

cv.error <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error

set.seed(17)
cv.error.10 <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
cv.error.10
