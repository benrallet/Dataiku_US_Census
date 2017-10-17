setwd("C:/Users/Bénédicte/Documents/Dataiku")
source("Scripts/data.R")
source("Scripts/separ1.R")
library(tree)

training <- read.csv("Data/census_income_learn.csv", header=F)
colnames(training) <- c("AAGE","ACLSWKR","ADTIND","ADTOCC", "AHGA", 
                        "AHRSPAY", "AHSCOL", "AMARITL", "AMJIND",
                        "AMJOCC", "ARACE", "AREORGN", "ASEX", "AUNMEM", 
                        "AUNTYPE", "AWKSTAT", "CAPGAIN", "CAPLOSS", "DIVVAL", 
                        "FILESTAT", "GRINREG", "GRINST", "HHDFMX", 
                        "HHDREL", "MARSUPWT", "MIGMTR1", "MIGMTR3", "MIGMTR4", 
                        "MIGSAME", "MIGSUN","NOEMP", "PARENT", 
                        "PEFNTVTY","PEMNTVTY","PENATVTY","PRCITSHP", 
                        "SEOTR", "VETQVA", "VETYN", "WKSWORK","YEAR", "INCOME")

# The attribute "instance weight" should *not* be used in the 
# classifiers, so it is set to "ignore" in this file
training <- subset(training, select=-c(MARSUPWT))
training <- data.modificationOfCountryOfBirth(training)
training <- subset(training, select=-c(GRINST,HHDFMX))

# 10-fold cross-validation
# Randomly shuffle the training data
training <- training[sample(nrow(training)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(training)), breaks=10, labels=FALSE)
folds

err <- matrix(nrow=10, ncol=1)

#Perform 10-fold cross validation
for(i in 1:10){
  print(i)
  testIndexes <- which(folds==i,arr.ind = TRUE)
  
  testData <- training[testIndexes, ]
  trainData <- training[-testIndexes, ]
  
  Xtest <- as.data.frame(testData)[,1:length(testData)-1]
  ztest <- factor(testData[,length(testData)])
  
  Xtrain <- as.data.frame(trainData)[,1:length(trainData)-1]
  ztrain <- factor(trainData[,length(trainData)])
  
  # Tree
  control_tree <- tree.control(nobs=dim(Xtrain)[1],mindev = 0.0001) # entire tree
  tr <- tree(ztrain ~ ., data.frame(Xtrain), control = control_tree) # zapp has to be factor
  
  validation <- cv.tree(tr, FUN = prune.misclass)
  prob <- predict(tr, Xtest)
  pred <- as.matrix(max.col(prob))
  err[i] <- sum(levels(ztest)[pred] != ztest)/length(ztest)
}

IC <- function(moy,var,n,alpha) {
  student <- qt(1-alpha/2, df = n-1)
  IC <- c(moy - student*sqrt(var/n), moy + student*sqrt(var/n))
  IC
}

err_mean <- mean(err)
var <- apply(err,1, function(x) ((x-err_mean)^2))
var <- 1/(10-1)*apply(matrix(var),2,sum)
IC <- IC(err_mean,var,10,0.05)



# Fit the model with all the training data
control_tree <- tree.control(nobs=dim(training[,1:38])[1],mindev = 0.0001) # entire tree
tr <- tree(as.factor(z) ~ ., data.frame(training[,1:38]), control = control_tree) # zapp has to be factor

validation <- cv.tree(tr, FUN = prune.misclass) 

# test file
test <- read.csv("Data/census_income_test.csv", header=F)
colnames(test) <- c("AAGE","ACLSWKR","ADTIND","ADTOCC", "AHGA", 
                        "AHRSPAY", "AHSCOL", "AMARITL", "AMJIND",
                        "AMJOCC", "ARACE", "AREORGN", "ASEX", "AUNMEM", 
                        "AUNTYPE", "AWKSTAT", "CAPGAIN", "CAPLOSS", "DIVVAL", 
                        "FILESTAT", "GRINREG", "GRINST", "HHDFMX", 
                        "HHDREL", "MARSUPWT", "MIGMTR1", "MIGMTR3", "MIGMTR4", 
                        "MIGSAME", "MIGSUN","NOEMP", "PARENT", 
                        "PEFNTVTY","PEMNTVTY","PENATVTY","PRCITSHP", 
                        "SEOTR", "VETQVA", "VETYN", "WKSWORK","YEAR", "INCOME")

test <- data.modificationOfCountryOfBirth(test)
ztest <- as.numeric(test[,"INCOME"])
test <- subset(test, select=-c(GRINST,HHDFMX,INCOME))

prob <- predict(tr, test) # classe un jeu de données au moyen de l'arbre
pred <- as.matrix(max.col(prob))
err <- sum(as.numeric(tr$y) != as.numeric(ztest))/length(ztest)

ztest <- as.matrix(ztest)

# confusion matrix
mc <- matrix(0, nrow=2,ncol=2)
mc[1,1] <- sum(pred[which(ztest==1)] == ztest[which(ztest==1)])
mc[1,2] <- sum(pred[which(ztest==1)] != ztest[which(ztest==1)])
mc[2,1] <- sum(pred[which(ztest==2)] != ztest[which(ztest==2)])
mc[2,2] <- sum(pred[which(ztest==2)] == ztest[which(ztest==2)])
mc
