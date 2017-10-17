# glmnet
library(glmnet)
library(caret)
#logistic <- glmnet(x=as.matrix(testFrame2), y=z, family="binomial", alpha=1)
#predict(logistic, as.matrix(testFrame2))

setwd("C:/Users/Bénédicte/Documents/Dataiku")

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


training <- training[sample(nrow(training)),]
z <- training[,"INCOME"]
training <- subset(training, select=-c(MARSUPWT))
training <- subset(training, select=-c(GRINST,HHDFMX,INCOME))
training <- data.modificationOfCountryOfBirth(training)
training <- data.modificationBinaryVariable(training)
dmy <- dummyVars(" ~ .", data = training)
training <- data.frame(predict(dmy, newdata = training))
training <- training[, !colnames(training) %in% colnames(training)[which(grepl( "Not.in.universe", colnames(training)))]]


#Create 10 equally size folds
folds <- cut(seq(1,nrow(training)), breaks=10, labels=FALSE)
folds

err <- matrix(nrow=10, ncol=1)

#Perform 10-fold cross validation
for(i in 1:10){
  print(i)
  testIndexes <- which(folds==i,arr.ind = TRUE)
  
  Xtest <- training[testIndexes,]
  ztest <- z[testIndexes]
  Xtrain <- training[-testIndexes,]
  ztrain <- z[-testIndexes]

  logistic2 <- cv.glmnet(x=as.matrix(Xtrain), y=ztrain, family="binomial", alpha=1, nfolds=5)

  pred <- as.factor(predict(logistic2, 
                            as.matrix(Xtest), 
                            s = "lambda.min",
                            type = "class"))
  err[i] <- sum(pred != ztest)/length(ztest)
  print(err[i])
}


logistic <- cv.glmnet(x=as.matrix(training), y=z, family="binomial", alpha=1, nfolds=5)
coef(logistic, s= "lambda.min")

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

ztst <- test[,"INCOME"]

# The attribute "instance weight" should *not* be used in the 
#classifiers, so it is set to "ignore" in this file
test <- subset(test, select=-c(MARSUPWT))
test <- subset(test, select=-c(GRINST,HHDFMX,INCOME))

test <- data.modificationOfCountryOfBirth(test)
test <- data.modificationBinaryVariable(test)
dmy <- dummyVars(" ~ .", data = test)
test <- data.frame(predict(dmy, newdata = test))

test <- test[, !colnames(test) %in% colnames(test)[which(grepl( "Not.in.universe", colnames(test)))]]

predtst <- as.factor(predict(logistic, 
                             as.matrix(test), 
                             s = "lambda.min",
                             type = "class"))
error <- sum(predtst != ztst)/length(ztst)
error

predtst <- as.numeric(predtst)
ztst <- as.numeric(ztst)

mc <- matrix(0, nrow=2,ncol=2)
mc[1,1] <- sum(predtst[which(ztst==1)] == ztst[which(ztst==1)])
mc[1,2] <- sum(predtst[which(ztst==1)] != ztst[which(ztst==1)])
mc[2,1] <- sum(predtst[which(ztst==2)] != ztst[which(ztst==2)])
mc[2,2] <- sum(predtst[which(ztst==2)] == ztst[which(ztst==2)])
mc

