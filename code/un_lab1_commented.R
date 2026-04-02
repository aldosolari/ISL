# =========================================
# Introduction to R
# =========================================

# --- Intro: R and RStudio ---

# "R is a programming language for statistics, data analysis, and visualization.
# RStudio is an environment that makes R easier to use.
# You write code in scripts, run it in the console,
# and view results, plots, and data in separate panels."

# --- Basic objects ---
x <- c(1, 3, 2, 5)
x

# "R works with vectors as its basic data structure.
# The function c() combines values into a vector.
# Typing the object name prints its contents."

x = c(1, 6, 2)
y = c(1, 4, 3)

# "We can reassign variables and create new ones.
# Assignment can be done with either <- or =."

length(x)
length(y)
x + y

# "length() gives the number of elements.
# Operations like addition are element-wise."


# --- Workspace management ---
ls()
rm(x, y)
ls()

# "ls() shows objects in memory.
# rm() removes selected objects."

rm(list = ls())

# "This removes everything from the workspace."


# --- Help system ---
?matrix

# "The help system is essential.
# Always check documentation when learning a new function."


# --- Matrices ---
x <- matrix(c(1, 2, 3, 4), 2, 2)
x

# "matrix() creates a matrix from a vector.
# By default, it fills by columns."

matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE)

# "Setting byrow = TRUE fills the matrix row by row."


# --- Element-wise operations ---
sqrt(x)
x^2

# "Functions like sqrt and power apply element-wise to matrices."


# --- Random data ---
x <- rnorm(50)
y <- x + rnorm(50, mean = 50, sd = 0.1)

cor(x, y)

# "rnorm() generates normal random variables.
# cor() measures linear relationship between variables."


# --- Reproducibility ---
set.seed(1303)
rnorm(50)

# "set.seed() ensures reproducibility.
# This is essential when working with randomness."


# --- Summary statistics ---
set.seed(3)
y <- rnorm(100)

mean(y)
var(y)
sd(y)

# "mean gives the average.
# var and sd measure variability."


# --- Plotting ---
x <- rnorm(100)
y <- rnorm(100)

plot(x, y)

# "plot() creates a scatterplot of two variables."

plot(x, y,
     xlab = "x",
     ylab = "y",
     main = "Scatterplot of x and y")

# "We can add labels and titles to improve readability."


# --- Saving plots ---
pdf("Figure.pdf")
plot(x, y, col = "green")
dev.off()

# "pdf() saves plots to a file.
# dev.off() closes the file."


# --- Sequences ---
x <- seq(1, 10)
x

x <- 1:10
x

x <- seq(-pi, pi, length = 50)

# "seq() generates sequences.
# The colon operator is a shortcut for integers."


# --- Functions on grids ---
y <- x
f <- outer(x, y, function(x, y) cos(y) / (1 + x^2))

# "outer() evaluates a function over all combinations of x and y."


# --- Visualization ---
contour(x, y, f)

fa <- (f - t(f)) / 2
image(x, y, fa)
persp(x, y, fa)

# "Different plots show different aspects of the function:
# contour for level curves, image for colors, and persp for 3D view."


# --- Indexing ---
A <- matrix(1:16, 4, 4)

A[2, 3]
A[1:2, ]
A[, 1:2]
A[-c(1, 3), ]

# "Indexing lets us extract parts of a matrix.
# Negative indices exclude elements."


# --- Reading data ---
Auto <- read.csv("../data/Auto.csv", na.strings = "?")

dim(Auto)
names(Auto)

Auto <- na.omit(Auto)

# "read.csv() loads data.
# dim() shows size, names() shows variables.
# na.omit() removes missing values."


# --- Data visualization ---
plot(Auto$cylinders, Auto$mpg)

# "We can access variables using the $ operator."


# --- Attach ---
attach(Auto)
plot(cylinders, mpg)

# "attach() allows direct variable names.
# Use carefully to avoid confusion."


# --- Categorical variables ---
cylinders <- as.factor(cylinders)
plot(cylinders, mpg)

# "Factors represent categorical variables.
# Plots change accordingly, showing group comparisons."


# --- Distributions ---
hist(mpg)
hist(mpg, col = 2, breaks = 15)

# "Histograms show the distribution of a variable."


# --- Relationships ---
pairs(Auto)

pairs(~ mpg + displacement + horsepower + weight + acceleration,
      data = Auto)

# "pairs() shows relationships between multiple variables."


# --- Scatterplot ---
plot(horsepower, mpg)

# "Scatterplots help identify relationships between variables."


# --- Summary ---
summary(Auto)
summary(mpg)

# "summary() gives a quick overview of the data."


# --- Final takeaway ---
# "R allows us to create objects, manipulate data, visualize relationships,
# and summarize information.
# These are the foundations before moving to statistical modeling."