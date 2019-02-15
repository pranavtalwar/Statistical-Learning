set.seed(5462)
# Part A
library(ISLR)
attach(Auto)
mpg01 = rep(0, length(mpg))
for (i in 1:length(mpg)){
  if(mpg[i] > median(mpg)){
    mpg01[i] = 1
  }
}
mpg01
Auto01 = data.frame(Auto, mpg01)
View(Auto01)

#Part B
pairs(Auto01[,-9]) # scatterplots of all variables in Auto01 except name
cor(Auto01[,-9]) #correlation matrix of variables in Auto01 except name
par(mfrow=c(2,3)) #exploring boxplots
boxplot(cylinders ~ mpg01, data = Auto, main = "Cylinders vs mpg01")
boxplot(displacement ~ mpg01, data = Auto, main = "Displacement vs mpg01")
boxplot(horsepower ~ mpg01, data = Auto, main = "Horsepower vs mpg01")
boxplot(weight ~ mpg01, data = Auto, main = "Weight vs mpg01")
boxplot(acceleration ~ mpg01, data = Auto, main = "Acceleration vs mpg01")
boxplot(year ~ mpg01, data = Auto, main = "Year vs mpg01")

# Using the analysis tools give nabove we can see that mpg01 is strongly related to "cylinders", "displacement", "horsepower" and "weight".

#Part C
#Splitting original data into training and testing data
train = sample(392, 0.8*dim(Auto01)[1])
test = -train
training_data = Auto01[train,] 
length(training_data[,1])
testing_data = Auto01[test,]
length(testing_data[,1])
mpg01.test = mpg01[test]
length(mpg01.test)


#Part D
glm.fits=glm(mpg01~cylinders+displacement+horsepower+weight , data=training_data ,family=binomial)
summary(glm.fits)
probability = predict(glm.fits, testing_data, type = "response")
intermediate.predictions = rep(0, length(probability))
intermediate.predictions[probability > 0.5] <- 1
table(intermediate.predictions, mpg01.test)
mean(intermediate.predictions!= mpg01.test)
#The test error of the model obtained is 13.92%
