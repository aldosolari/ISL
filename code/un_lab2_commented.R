# =========================================
# Regression in R
# =========================================

# --- Setup ---
library(MASS)
library(ISLR2)
head(Boston)

# "Today we’re going to study regression in R.
# We’ll start with simple linear regression, then increase model flexibility,
# and finally evaluate models using validation.
# Let’s begin by looking at the Boston housing data."


# --- Linear Regression ---
lm.fit <- lm(medv ~ lstat, data = Boston)
summary(lm.fit)

# "We fit a linear model where median house value is explained by lstat.
# The coefficient is negative, so higher lstat is associated with lower house values.
# The p-value is very small, so this relationship is statistically significant.
# R-squared tells us how much variability is explained."


# --- Model Details ---
names(lm.fit)
coef(lm.fit)
confint(lm.fit)

# "We can inspect the model object.
# The coefficients define the fitted line.
# The confidence intervals show the uncertainty around these estimates."


# --- Prediction ---
predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = "confidence")
predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = "prediction")

# "There are two types of intervals.
# Confidence intervals are for the mean response.
# Prediction intervals are for new observations, so they are wider."


# --- Visualization ---
attach(Boston)
plot(lstat, medv)
abline(lm.fit)
abline(lm.fit, lwd = 3, col = "red")

# "We plot the data and the fitted line.
# The relationship is not perfectly linear.
# There is some curvature, so we may need a more flexible model."


# --- Polynomial Regression ---
lm.fit2 <- lm(medv ~ lstat + I(lstat^2), data = Boston)
summary(lm.fit2)
anova(lm.fit, lm.fit2)

# "We add a quadratic term to capture curvature.
# The I() function tells R to treat lstat squared as arithmetic.
# We compare models using ANOVA.
# This tests whether the more complex model improves the fit."


# --- Diagnostics ---
par(mfrow = c(2,2))
plot(lm.fit2)

# "These plots help check assumptions like linearity and constant variance.
# In practice, we always check diagnostics."


# --- More Flexible Model ---
lm.fit5 <- lm(medv ~ poly(lstat, 5), data = Boston)
summary(lm.fit5)

# "We can increase flexibility further.
# But more flexibility can lead to overfitting.
# So we need to be careful."


# --- Train/Test Split ---
set.seed(1)
train <- sample(392, 196)

lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)

# "We now evaluate performance on unseen data.
# We train on one part of the data and test on the rest.
# This gives us the test error, which reflects real performance."


# --- Model Comparison ---
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)

mean((mpg - predict(lm.fit2, Auto))[-train]^2)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

# "We compare models of different complexity.
# We choose the model with the lowest test error.
# This balances underfitting and overfitting."


# --- Cross-Validation ---
library(boot)

glm.fit <- glm(mpg ~ horsepower, data = Auto)
cv.glm(Auto, glm.fit)$delta

# "Instead of one split, we can use cross-validation.
# Leave-one-out fits the model many times,
# each time leaving out one observation."


# --- Model Selection with CV ---
cv.error <- rep(0, 10)

for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}

cv.error

# "We compute cross-validation error for models of different complexity.
# Typically, quadratic improves a lot, higher degrees not much."


# --- 10-fold Cross-Validation ---
set.seed(17)

cv.error.10 <- rep(0, 10)

for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}

cv.error.10

# "10-fold cross-validation is faster and more stable.
# In practice, it is usually preferred."


# --- Final Takeaway ---
# "Start simple.
# Add flexibility carefully.
# Always evaluate using validation.
# The goal is good prediction on new data, not just fitting the training data."