source("Scripts/data.R")

training <- read.csv("Data/census_income_learn.csv", header=F)
training <- data.modificationOfCountryOfBirth(training)
z <- as.numeric(training$V42)
max(z)
training <- training[,c(1:21,24:41)]

donn.sep <- separ1(training, z) # separation des données 
Xapp <- as.data.frame(donn.sep$Xapp)
zapp <- factor(donn.sep$zapp)
Xtst <- donn.sep$Xtst
ztst <- donn.sep$ztst

control_tree <- tree.control(nobs=dim(Xapp)[1],mindev = 0.0001) # entire tree
tr <- tree(zapp~., data.frame(Xapp), control = control_tree) # zapp has to be factor

validation <- cv.tree(tr, FUN = prune.misclass) # séquence d'arbres emboités + estimation d'erreur par validation croisée
#prune.misclass correspond à l'élagage élagage
prob <- predict(tr, Xtst) # classe un jeu de données au moyen de l'arbre
pred <- as.matrix(max.col(prob))
err <- sum(pred != ztst)/length(ztst)

ztst <- as.matrix(ztst)

# confusion matrix
mc <- matrix(0, nrow=2,ncol=2)
mc[1,1] <- sum(pred[which(ztst==1)] == ztst[which(ztst==1)])
mc[1,2] <- sum(pred[which(ztst==1)] != ztst[which(ztst==1)])
mc[2,1] <- sum(pred[which(ztst==2)] != ztst[which(ztst==2)])
mc[2,2] <- sum(pred[which(ztst==2)] == ztst[which(ztst==2)])

# test file
test <- read.csv("Data/census_income_test.csv", header=F)
test <- data.modificationOfCountryOfBirth(test)
ztest <- as.numeric(test$V42)

prob <- predict(tr, test) # classe un jeu de données au moyen de l'arbre
pred <- as.matrix(max.col(prob))
err <- sum(pred != ztest)/length(ztest)

ztest <- as.matrix(ztest)

# confusion matrix
mc <- matrix(0, nrow=2,ncol=2)
mc[1,1] <- sum(pred[which(ztest==1)] == ztest[which(ztest==1)])
mc[1,2] <- sum(pred[which(ztest==1)] != ztest[which(ztest==1)])
mc[2,1] <- sum(pred[which(ztest==2)] != ztest[which(ztest==2)])
mc[2,2] <- sum(pred[which(ztest==2)] == ztest[which(ztest==2)])
mc
