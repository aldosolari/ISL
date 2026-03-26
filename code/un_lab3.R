library(MASS)
lm.fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)

lm.fit <- lm(medv ~ ., data = Boston)
summary(lm.fit)

library(car)
vif(lm.fit)

lm.fit1 <- lm(medv ~ . - age, data = Boston)
summary(lm.fit1)

lm.fit1 <- update(lm.fit, ~ . - age)

summary(lm(medv ~ lstat * age, data = Boston))

library(ISLR2)
head(Carseats)

lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age, 
    data = Carseats)
summary(lm.fit)

attach(Carseats)
contrasts(ShelveLoc)

LoadLibraries
LoadLibraries()

LoadLibraries <- function() {
 library(ISLR2)
 library(MASS)
 print("The libraries have been loaded.")
}

LoadLibraries

LoadLibraries()

library(ISLR2)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

library(leaps)
regfit.full <- regsubsets(Salary ~ ., Hitters)
summary(regfit.full)

regfit.full <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19)
reg.summary <- summary(regfit.full)

names(reg.summary)

reg.summary$rsq

par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Number of Variables",
    ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables",
    ylab = "Adjusted RSq", type = "l")

which.max(reg.summary$adjr2)
plot(reg.summary$adjr2, xlab = "Number of Variables",
    ylab = "Adjusted RSq", type = "l")
points(11, reg.summary$adjr2[11], col = "red", cex = 2, 
    pch = 20)

plot(reg.summary$cp, xlab = "Number of Variables",
    ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col = "red", cex = 2,
    pch = 20)
which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables",
    ylab = "BIC", type = "l")
points(6, reg.summary$bic[6], col = "red", cex = 2,
    pch = 20)

plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")

coef(regfit.full, 6)

regfit.fwd <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19, method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19, method = "backward")
summary(regfit.bwd)

coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters),
    replace = TRUE)
test <- (!train)

regfit.best <- regsubsets(Salary ~ .,
    data = Hitters[train, ], nvmax = 19)

test.mat <- model.matrix(Salary ~ ., data = Hitters[test, ])

val.errors <- rep(NA, 19)
for (i in 1:19) {
 coefi <- coef(regfit.best, id = i)
 pred <- test.mat[, names(coefi)] %*% coefi
 val.errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}

val.errors
which.min(val.errors)
coef(regfit.best, 7)

predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

regfit.best <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19)
coef(regfit.best, 7)

k <- 10
n <- nrow(Hitters)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, 19,
    dimnames = list(NULL, paste(1:19)))

for (j in 1:k) {
  best.fit <- regsubsets(Salary ~ .,
       data = Hitters[folds != j, ],
       nvmax = 19)
  for (i in 1:19) {
    pred <- predict(best.fit, Hitters[folds == j, ], id = i)
    cv.errors[j, i] <-
         mean((Hitters$Salary[folds == j] - pred)^2)
   }
 }

mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b")

reg.best <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19)
coef(reg.best, 10)
