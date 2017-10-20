library(randomForest)

set.seed(100)
model <- randomForest(formula = as.factor(z) ~ ., data = training[,1:36])

p <- model$predicted
err <- sum(p != z)/length(z)
importantce <- model$importance

# the method doesn't work because of size restriction from R
 