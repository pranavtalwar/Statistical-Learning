# Question 1
set.seed(5462)

# Part a
X = rnorm(100,0,1)
X
# Part b
eps = rnorm(100,mean = 0,sd = 0.5)
eps

# Part c
Y = -1 + 0.5*X + eps
Y
length(Y)
# The length of vector Y is 100, ??0 is -1 and ??1 is 0.5.

# Part d
plot(X,Y)
# On observing the scatterplot between X and Y we can observe that there seems to be some sort of correlation betwee the two vsriables. 
# We can observe that Y maybe dependent on X as when X increases Y is also increasing hence we can say that they have positive correlation. On viewing the scatterplot we can also notice that the relationship between Y and X can be modelled using a linear model.

# Part E
lm.fit = lm(Y~X)
summary(lm.fit)
coef(lm.fit)
# We can observe from the linear model that the values of ??0 and ??1 are -1.0359026 and 0.5205766  respectively. 
# From these two values we can say that Y increases by 0.5205766 when X is increased by one unit.
# Furthermore, the p-value of x is <0.05 hence we reject the null hypothesis and conclude that there is significant correlation between X and Y.
#These values are very close to the original values of ??0 and ??1, hence we can say that the least squares line is a good fit to the data.

# Part F
abline(lm.fit, col="red")
legend(0.9,-1.9,c("Least Squares Line","Population Regression Line"), fill=c("red", "green"), title="Legend")
abline(-1,0.5, col = "green")


# Part G
lm.fit2 = lm(Y~poly(X,2))
summary(lm.fit2)
par(new=TRUE)
plot(function(x) -1.06873 + 5.41132*x + -0.66692*(x*x))
mean((Y-predict(lm.fit))^2)
mean((Y-predict(lm.fit2))^2)
# The training MSE of the two models is 0.3289078 (linear model) and 0.3201902 (polynomial model). Hence we can see that the polynomial
# model does not provide a significant improvement as compared to the linear model. The same is displayed by the Residual standard error of both models, 
# where there is not much of a difference 


#Part H
X = rnorm(100,0,1)
eps = rnorm(100,0,0.1)
Y = -1 + 0.5*X + eps
plot(X,Y)
lm.fit2 = lm(Y~X)
summary(lm.fit2)
abline(lm.fit2, col="red")
legend(0.4,-1,c("Least Squares Line","Population Regression Line"), fill=c("red", "green"), title="Legend")
abline(-1,0.5, col = "green")
# In this case, the population regression and the least squares line are very close to each other and almost overlap.
# Due to a decrease in the variance of the error term eps we can see that data points are closer to each other and fit a linear model better
# This can be seen by the values of the RSE, which has decreased considerably as compared to when the variance of the error term was high 
# and also by the increase in the R-squared statistic which is 0.969 (when the variance in the error term eps is 0.01).

#Part I
X = rnorm(100,0,1)
eps = rnorm(100,0,0.6)
Y = -1 + 0.5*X + eps
plot(X,Y)
lm.fit3 = lm(Y~X)
summary(lm.fit3)
abline(lm.fit3, col="red")
legend(0.6,-2,c("Least Squares Line","Population Regression Line"), fill=c("red", "green"), title="Legend")
abline(-1,0.5, col = "green")
# In this case, the population line and the least squares line are not as close to each other and do not overlap as compared to previous cases.
# Due to an increase in the variance of the error term eps we can see that data points are far apart from each other and might not fit a linear model that well.
# The following is also represented by the value of the RSE, which has increased considerably as compared to when the variance of the error term was lower
# It is also reflected by the decrease in the R-squared statistic which is 0.379 (when the variance of the error term is 0.49 )

#Part J
confint(lm.fit) # original dataset
confint(lm.fit2) # less noisy dataset
confint(lm.fit3) # more noisy dataset
# From the above data we can see that when the range of the confidence interval is low we can predict the value of the intercept and the coefficient of x with greater accuracy as compared to when the range of the confidence interval is high.