# Part A
set.seed(5462)
library(ISLR)
library(tree)
library(randomForest)
nrow(Carseats)
train = sample(400,200)
train
Carseats.train = Carseats[train,]
Carseats.test = Carseats[-train,]



#Part B
carseats.tree = tree(Sales~.,data=Carseats.train)
plot(carseats.tree)
text(carseats.tree, pretty = 0)
summary(carseats.tree)
test_results.tree= predict(carseats.tree, newdata= Carseats.test)
mean((test_results.tree- Carseats.test$Sales)^2)

#Part C
carseats.cv = cv.tree(carseats.tree)
plot(carseats.cv$size, carseats.cv$dev, type="b")


prune.carseats = prune.tree(carseats.tree, best = 6)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
test_results.prune = predict(prune.carseats, newdata= Carseats.test)
mean((test_results.prune - Carseats.test$Sales)^2)


#Part D
carseats.bagging = randomForest(Sales ~ ., data = Carseats.train, mtry = 10, ntree = 500, importance = TRUE)
test_results.bagging = predict(carseats.bagging, newdata = Carseats.test)
mean((test_results.bagging - Carseats.test$Sales)^2)

importance(carseats.bagging)


#Part E
carseats.rf = randomForest(Sales ~ ., data = Carseats.train, mtry = 3, ntree = 500, importance = TRUE)
test_result.rf = predict(carseats.rf, newdata = Carseats.test)
mean((test_result.rf - Carseats.test$Sales)^2)
importance(carseats.rf)
