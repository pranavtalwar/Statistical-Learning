#Question 4
set.seed(5462)
library(MASS)
library(stringr)


#Part A
attach(Boston)
medv.mean = mean(medv)
medv.mean
# ???? (Population mean of medv) is 22.5381.

# Part B
medv.se = sd(medv)/sqrt(length(medv))
medv.se
# The standard error of ???? is 0.4088611.

# Part C
boot.fn = function(data, index){
  return (mean(data[index]))
}
medv.boot = boot(medv,boot.fn,1000)
x = capture.output(medv.boot) # store the output as text
x = str_extract(x ,"^t1.*$") # grab the line that starts with t1
x = x[!is.na(x)] # remove all the lines we don't need
medv.se.bootstrap = as.numeric(unlist(str_extract_all(x, '[0-9.]+$')))
medv.se.bootstrap
# The estimate of standard error of ???? is 0.4056646 using the bootstrap method which is extremely close to 0.4088611 which was found in part B.

# Part D
t.test(medv)
CI.medv.se = c(medv.mean - 2*medv.se.bootstrap, medv.mean + 2*medv.se.bootstrap )
CI.medv.se
# The confidence interval generated by the t-test is (21.72953, 23.33608) while that generated with the help of the bootstrap estimate 
# of the standard error is (21.70348, 23.36213). Hence we can see that the bootstrap confidence interval is very close to the one provided by the t-test() function.

#Part E
medv.median = median(medv)
medv.median
# ????med (Population median of medv) is 21.2.

#Part F
boot.fn2 = function(data, index){
  return (median(data[index]))
}
boot(medv,boot.fn2, 1000)
# Using bootstrap the standard error of the ????med comes out to be 0.378404 which is extremely small when compared to the value of the median which is 21.2 (given by bootstrap).

# Part G
quantile(medv, 0.1)
# The tenth percentile for medv in the Boston suburbs is 12.75

# Part I
boot.fn3 = function(data, index){
  return (quantile(data[index], 0.1))
}
boot(medv, boot.fn3, 1000)
# ??0.1 is 12.7 with a standard error of 0.511. The standard error is small relative to the value of ??0.1.