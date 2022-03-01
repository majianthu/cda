library(copent)
library(mnormt)
library(copula)

# normal distribution
n1 = 200
n2 = 300
rho1 = 0.9
rho2 = 0.5
rho3 = 0.8
mu0 = c(0,0)
mu1 = c(1,1)
sigma1 = matrix(c(1,rho1,rho1,1),2,2)
sigma2 = matrix(c(1,rho2,rho2,1),2,2)
sigma = matrix(c(1,rho3,rho3,1),2,2)
x1a = rmnorm(n1, mu0, sigma1)
x2a = rmnorm(n2, mu1, sigma2)
x3a = rnorm(n1+n2)
xa = rbind(x1a,x2a)
ya = pmnorm(xa,mu0,sigma)
rho = c(rep(rho1,n1),rep(rho2,n2)) + 0.0000005 * runif(n1+n2)
ci1a = ci(xa[,1],ya,rho)
ci2a = ci(xa[,2],ya,rho)
ci3a = ci(x3a,ya,rho)
cia = c(ci1a,ci2a,ci3a)

## plot simulate data
x11()
plot(xa, col = "white", xlab = "x1", ylab = "x2")
points(x1a, col = 2, pch = 1)
points(x2a, col = 3, pch = 2)
legend(min(xa[,1]),max(xa[,2]), legend = c(expression(paste(rho,"=0.9")),expression(paste(rho,"=0.5"))), col = c(2,3), pch = c(1,2), bg = 7)

## plot test results
x11()
names(cia) = c("x1","x2","x3")
min1 = min(cia)
if (min1>0) min1 = 0.035
xci1 = barplot(cia, col = "grey", border = "grey", ylab = "CI", ylim = c(min1-0.035, max(cia)+0.035))
text(cex = 1, x = xci1, y = cia+ sign(cia) * 0.02, labels = round(cia,digits = 4), col = "red")

## copula functions
n1 = 300
n2 = 500
theta1 = 0.3
cl1 = claytonCopula(theta1)
x1b = rCopula(n1, cl1)
theta2 = 3
cl2 = claytonCopula(theta2)
x2b = rCopula(n2, cl2)
x3b = runif(n1+n2)
xb = rbind(x1b,x2b)
fc1 = frankCopula(0.5)
yb = pCopula(xb,fc1)
theta = c(rep(theta1,n1),rep(theta2,n2)) + 0.000005 * runif(n1+n2)
ci1b = ci(yb,xb[,1],theta)
ci2b = ci(yb,xb[,2],theta)
ci3b = ci(yb,x3b,theta)
cib = c(ci1b,ci2b,ci3b)

## plot simulate data
x11()
plot(x1b, col = "white", xlab = "x1", ylab = "x2")
points(x1b, col = 2, pch = 1)
points(x2b, col = 3, pch = 2)
legend(0,1, legend = c(expression(paste(theta,"=0.3")),expression(paste(theta,"=3.0"))), col = c(2,3), pch = c(1,2), bg = 7)

## plot test results
x11()
names(cib) = c("x1","x2","x3")
min1 = min(cib)
if (min1>0) min1 = 0.035
xci1 = barplot(cib, col = "grey", border = "grey", ylab = "CI", ylim = c(min1-0.035, max(cib)+0.035))
text(cex = 1, x = xci1, y = cib+ sign(cib) * 0.02, labels = round(cib,digits = 4), col = "red")
