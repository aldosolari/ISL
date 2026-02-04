library(ggpubr)
library(modeldata)
data(deliveries, package = "modeldata")
deliveries$distance <- 1.60934*deliveries$distance
head(as.data.frame(deliveries[,c(1:8,ncol(deliveries))] ))

gghistogram(deliveries, 
            x = "time_to_delivery", 
            fill = "lightgray", 
            rug = FALSE, 
            xlab = "Time Until Deliver (min)")

ggscatter(deliveries,
          alpha = 0.1,
          add = "reg.line",
          add.params = list(color = "blue", fill = "lightgray"),
            y = "time_to_delivery", 
            x = "distance",
          xlab = "Distance (km)",
            ylab = "Time Until Deliver (min)")

ggscatter(deliveries,
          alpha = 0.1,
            y = "time_to_delivery", 
            x = "hour",
          xlab = "Order Time (decimal hours)",
            ylab = "Time Until Deliver (min)")

library(plotly)

# Fit linear model
fit <- lm(time_to_delivery ~ hour + distance, data = deliveries)

# 3D scatter with alpha
p <- plot_ly(deliveries,
  x = ~hour,
  y = ~distance,
  z = ~time_to_delivery,
  type = "scatter3d",
  mode = "markers",
  marker = list(
    size = 3,
    color = "black",
    opacity = 0.4
  )
)

# Grid for regression plane
hx <- seq(min(deliveries$hour), max(deliveries$hour), length.out = 30)
dy <- seq(min(deliveries$distance), max(deliveries$distance), length.out = 30)
grid <- expand.grid(hour = hx, distance = dy)

z <- matrix(
  predict(fit, newdata = grid),
  nrow = length(hx),
  ncol = length(dy)
)

# Add blue transparent plane
p <- add_surface(
  p,
  x = hx,
  y = dy,
  z = z,
  opacity = 0.5,
  colorscale = list(c(0, 1), c("blue", "blue")),
  showscale = FALSE
)

# Axis labels
p <- layout(
  p,
  scene = list(
    xaxis = list(title = "Order Time (decimal hours)"),
    yaxis = list(title = "Distance (km)"),
    zaxis = list(title = "Time Until Deliver (min)")
  )
)

p

library(mgcv)

fit_spline <- gam(time_to_delivery ~ s(hour, distance), data = deliveries)

# grid to evaluate the smooth surface
hx <- seq(min(deliveries$hour), max(deliveries$hour), length.out = 40)
dy <- seq(min(deliveries$distance), max(deliveries$distance), length.out = 40)
grid <- expand.grid(hour = hx, distance = dy)

z <- matrix(predict(fit_spline, newdata = grid),
            nrow = length(hx), ncol = length(dy))

p <- plot_ly(deliveries,
  x = ~hour, y = ~distance, z = ~time_to_delivery,
  type = "scatter3d", mode = "markers",
  marker = list(size = 3, color = "black", opacity = 0.4)
)

p <- add_surface(p,
  x = hx, y = dy, z = z,
  opacity = 0.5,
  colorscale = "Blues",
  showscale = FALSE
)

p <- layout(
  p,
  scene = list(
    xaxis = list(title = "Order Time (decimal hours)"),
    yaxis = list(title = "Distance (km)"),
    zaxis = list(title = "Time Until Deliver (min)")
  )
)

p

library(rpart)
library(rpart.plot)
fit <- rpart(time_to_delivery~hour + distance, deliveries, control =rpart.control(surrogatestyle = 0, maxdepth = 2))
rpart.plot(fit, type=5, extra=0)

library(parttree) 
plot(parttree(fit), raw = FALSE)
text(x=12,y=10, "17 min")
text(x=14,y=10, "22 min")
text(x=18,y=4.5, "28 min")
text(x=18,y=12, "36 min")

df <- deliveries

# Step-function prediction
df$step_pred <- with(df,
  ifelse(hour < 13, 17,
  ifelse(hour < 15, 22,
  ifelse(distance < 6.4, 28, 36)))
)

df$step_pred <- predict(fit)

# 3D scatter
p <- plot_ly(df,
  x = ~hour,
  y = ~distance,
  z = ~time_to_delivery,
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 3, color = "black", opacity = 0.4)
)

# Grid for step surface
hx <- seq(min(df$hour), max(df$hour), length.out = 60)
dy <- seq(min(df$distance), max(df$distance), length.out = 60)
grid <- expand.grid(hour = hx, distance = dy)

# Apply step rule to grid
z_step <- with(grid,
  ifelse(hour < 13, 17,
  ifelse(hour < 15, 22,
  ifelse(distance < 6.4, 28, 36)))
)

z_step <- matrix(z_step, nrow = length(hx), ncol = length(dy))

# Add step surface
p <- add_surface(
  p,
  x = hx,
  y = dy,
  z = z_step,
  opacity = 0.6,
  colorscale = list(
    c(0, 0.33, 0.66, 1),
    c("lightblue", "deepskyblue", "royalblue", "navy")
  ),
  showscale = FALSE
)

# Axis labels
p <- layout(
  p,
  scene = list(
    xaxis = list(title = "Order Time (decimal hours)"),
    yaxis = list(title = "Distance (km)"),
    zaxis = list(title = "Time Until Deliver (min)")
  )
)

p

data("heptathlon", package = "HSAUR")
library(kableExtra)
kable(heptathlon, format = "html") %>%
  column_spec(ncol(heptathlon)+1, color = "red")

face <- read.table("https://raw.githubusercontent.com/aldosolari/AE/master/docs/dati/face.txt", header=FALSE)
X = as.matrix(face)
n = nrow(face)
p = ncol(face)
mat <- apply(X, 2, rev)
image(t(mat), col=gray(0:255/255), asp=p/n, xaxt="n", yaxt="n", frame.plot=F)
#image(X, col=gray(0:255/255), asp=p/n, xaxt="n", yaxt="n")

X[1:10,1:7]

pca = princomp(X, cor=F)
V = pca$loadings
Y = pca$scores
xbar = matrix(pca$center, ncol=1)
q = 10
Yq = Y[,1:q]
Vq = V[,1:q]
Aq = Yq %*% t(Vq)
one.n = matrix(rep(1,n), ncol=1)
face2 = Aq + one.n %*% t(xbar)
face2 <- pmax(pmin(face2, 1), 0)
mat2 <- apply(face2, 2, rev)
image(t(mat2), col=gray(0:255/255), asp=p/n, xaxt="n", yaxt="n", frame.plot=F)
