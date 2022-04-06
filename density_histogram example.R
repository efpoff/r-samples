#Histogram Example
#Simulate a random sample from the dist. with density fX(x) = 3x^ 2, 0 < x < 1. Here FX(x) = x^3
#for 0 < x < 1, and F−1sub X (u) = u^1/3, then
n <- 1000
u <- runif(n)
x <- u^(1/3)
hist(x, prob = TRUE, main = expression(f(x) == 3 * x^2)) #density histogram of sample
y <- seq(0, 1, 0.01)
lines(y, 3 * y^2) #density curve f(x)