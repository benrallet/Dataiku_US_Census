library(randomForest)

set.seed(100)
model <- randomForest(formula = as.factor(z[1:10000]) ~ ., data = training[1:10000,1:38])

p <- model$predicted
err <- sum(p != z[1:10000])/length(z[1:10000])
importantce <- model$importance
 