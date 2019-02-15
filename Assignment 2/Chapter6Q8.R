set.seed(5462)
library(leaps)
library(glmnet)
# Part A
X = rnorm(100)
e = rnorm(100)
X
e

# Part B
Y = 1 + X + X*X + X*X*X + e
data.new = data.frame(x= X,y = Y)
plot(data.new)

# Part C
subsets = regsubsets(y~poly(x,10, raw = TRUE),data = data.new,nvmax = 10)
subsets.summary = summary(subsets)
which.min(subsets.summary$cp)
which.min(subsets.summary$bic)
which.max(subsets.summary$adjr2)
par(mfrow=c(2,2))
plot(subsets.summary$cp, xlab = "Size of the Subset", ylab ="CP", type = "l")
points(4, subsets.summary$cp[4],col = "red")
plot(subsets.summary$bic, xlab = "Size of the Subset", ylab = "BIC", type ="l")
points(4, subsets.summary$bic[4],col = "red")
plot(subsets.summary$adjr2, xlab = "Size of the Subset", ylab ="ADJR2", type = "l")
points(8, subsets.summary$adjr2[8],col = "red")
coefficients(subsets, id = 4)
coefficients(subsets, id = 8)
par(mfrow=c(2,2))
plot(subsets, scale="Cp")
plot(subsets, scale="bic")
plot(subsets, scale="adjr2")


#Part D
subsets.fwd = regsubsets(y~poly(x,10, raw = TRUE), data = data.new, nvmax=10, method = "forward")
subsets.fwd.summary = summary(subsets.fwd)
which.min(subsets.fwd.summary$cp)
which.min(subsets.fwd.summary$bic)
which.max(subsets.fwd.summary$adjr2)
par(mfrow=c(2,2))
plot(subsets.fwd.summary$cp, xlab = "Size of the Subset", ylab ="CP", type = "l")
points(4, subsets.fwd.summary$cp[4],col = "red")
plot(subsets.fwd.summary$bic, xlab = "Size of the Subset", ylab = "BIC", type ="l")
points(4, subsets.fwd.summary$bic[4],col = "red")
plot(subsets.fwd.summary$adjr2, xlab = "Size of the Subset", ylab ="ADJR2", type = "l")
points(9, subsets.fwd.summary$adjr2[9],col = "red")
coefficients(subsets.fwd, id = 4)
coefficients(subsets.fwd, id = 9)



subsets.bwd = regsubsets(y~poly(x,10, raw = TRUE), data = data.new, nvmax=10, method = "backward")
subsets.bwd.summary = summary(subsets.fwd)
which.min(subsets.bwd.summary$cp)
which.min(subsets.bwd.summary$bic)
which.max(subsets.bwd.summary$adjr2)
par(mfrow=c(2,2))
plot(subsets.bwd.summary$cp, xlab = "Size of the Subset", ylab ="CP", type = "l")
points(4, subsets.bwd.summary$cp[4],col = "red")
plot(subsets.bwd.summary$bic, xlab = "Size of the Subset", ylab = "BIC", type ="l")
points(4, subsets.bwd.summary$bic[4],col = "red")
plot(subsets.bwd.summary$adjr2, xlab = "Size of the Subset", ylab ="ADJR2", type = "l")
points(9, subsets.bwd.summary$adjr2[9],col = "red")
coefficients(subsets.bwd, id = 4)
coefficients(subsets.bwd, id = 9)
plot(subsets.bwd, scale="Cp")


# Part E
par(mfrow=c(1,1))
xmat = model.matrix(y ~ poly(x, 10, raw = T), data = data.new)[, -1]
lasso.mod = cv.glmnet(xmat, Y, alpha = 1)
best.lambda = lasso.mod$lambda.min
best.lambda
plot(lasso.mod)
best.model = glmnet(xmat, Y, alpha = 1)
predict(best.model, s = best.lambda, type = "coefficients")

#Part F
Y2 = 1 + X^7 + e
data.new = data.frame(y = Y2, x = X)
regfit.7 <- regsubsets(y~poly(x,10,raw=T), data=data.new, nvmax=10)
reg.summary = summary(regfit.7)
which.min(reg.summary$cp)
which.min(reg.summary$bic)
which.min(reg.summary$adjr2)
coefficients(regfit.7, id=6)
coefficients(regfit.7, id=3)
coefficients(regfit.7, id=1)

xmat = model.matrix(y ~ poly(x, 10, raw = T), data = data.new)[, -1]
lasso.mod = cv.glmnet(xmat, Y, alpha = 1)
best.lambda = lasso.mod$lambda.min
best.lambda
best.model = glmnet(xmat, Y, alpha = 1)
predict(best.model, s = best.lambda, type = "coefficients")

