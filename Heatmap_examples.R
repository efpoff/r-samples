#Data, Covariance and Correlation Matrix from U of Minn
# http://users.stat.umn.edu/~helwig/notes/datamat-Notes.pdf
#Examples of different visualization methods using mtcars

#Install packages - the following packages are needed to run this:
#install.packages("car")
#install.packages("bigsplines")
#install.packages("corrplot")
#install.packages("RColorBrewer")

#Clear Environment
rm(list=ls())

#Data MTCARS
data(mtcars)
class(mtcars)
head(mtcars)

X <- as.matrix(mtcars)
class(X)

#get row means Method 1
rowMeans(X) [1:3]

#get row means Method 2
c(mean(X[1,]), mean(X[2,]),mean(X[3,]))

#get row means Method 3
apply(X,1,mean)[1:3]

#get column means Method 1
colMeans(X) [1:3]

# get column ranges
apply(X,2,range)[,1:3]

#Covariance Method 1
n <- nrow(X)
C <- diag(n) - matrix(1/n, n, n)
Xc <- C %*% X
S <- t(Xc) %*% Xc / (n-1)
S[1:3,1:6]

#Covariance the easy way
S <- cov(X)
dim(S)

#Check Variance
S[1,1]

#Check covariance
S[1:3,1:6]

#Correlation Method 1
n <- nrow(X)
C <- diag(n) - matrix(1/n, n, n)
D <- diag(apply(X, 2, sd))
Xs <- C %*% X %*% solve(D)
R <- t(Xs) %*% Xs / (n-1)
R[1:3,1:6]

#Correlation the Easy Way
# calculate correlation matrix
R <- cor(X)
dim(R)

# check correlations
R[1:3,1:6]

#Crossproducts - Method 1
X <- matrix(rnorm(2*3),2,3)
Y <- matrix(rnorm(2*3),2,3)
t(X) %*% Y

#Crossproducts - Easy
crossprod(X, Y)

#Turning a matrix into a vector
X <- matrix(1:6,2,3)
c(X)
c(t(X))

#Scatterplots in R
plot(mtcars$hp, mtcars$mpg, xlab="HP", ylab="MPG")
scatterplot(mtcars$hp, mtcars$mpg, xlab="HP", ylab="MPG")

#Two versions of Scatterplot Matrix
cylint <- as.integer(factor(mtcars$cyl))
pairs(~mpg+disp+hp+wt, data=mtcars, col=cylint, pch=cylint)
library(car)
scatterplotMatrix(~mpg+disp+hp+wt|cyl, data=mtcars)

#Heatmap Example
fitmod <- lm(mpg ~ hp + wt, data=mtcars)
hpseq <- seq(50, 330, by=20)
wtseq <- seq(1.5, 5.4, length=15)
newdata <- expand.grid(hp=hpseq, wt=wtseq)
fit <- predict(fitmod, newdata)
fitmat <- matrix(fit, 15, 15)
image(hpseq, wtseq, fitmat, xlab="HP", ylab="WT")
library(bigsplines)
imagebar(hpseq, wtseq, fitmat, xlab="HP", ylab="WT", zlab="MPG", col=heat.colors(12), ncolor=12)

#Correlation Example
cmat <- cor(mtcars)
library(corrplot)
corrplot(cmat, method="circle")
corrplot.mixed(cmat, lower="number", upper="ellipse")

cmat <- cor(mtcars)
p <- nrow(cmat)
library(RColorBrewer)
imagebar(1:p, 1:p, cmat[,p:1], axes=F, zlim=c(-1,1), xlab="", ylab="", col=brewer.pal(7, "RdBu"))
axis(1, 1:p, labels=rownames(cmat))
axis(2, p:1, labels=colnames(cmat))
for(k in 1:p) { for(j in 1:k) { if(j < k) text(j, p+1-k, labels=round(cmat[j,k],2), cex=0.75) } }