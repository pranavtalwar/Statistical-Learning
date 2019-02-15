# Question 3
set.seed(5462)
# Part A
library(ISLR)
library(boot)
attach(Default)
glm.fit = glm(default~income+balance,family=binomial, data=Default)
summary(glm.fit)
# The standard error for the coefficient of income is 4.985e-06 and that of the coefficient of balance is 2.274e-04.

# Part B
boot.fn = function(data,index){
  glm.fit = glm(default~income+balance,data=data,family=binomial,subset=index)
  return(coef(glm.fit))
}

#Part C
boot(Default,boot.fn,200)
#The standard error of coefficient estimate is calculated using library boot and boot.fn function defined above
# The standard error for the coefficient of income is 4.867277e-06 and that of the coefficient of balance is 2.219866e-04 using this method.

#Part D
#The standard error of coefficient estimates found from the glm() method and the bootstrap method are extremely close to each other.

