# Logistic Regression
library(glmnet)
library(caret)

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

# Pre-processing
# Remove varaibles (justification in the report)
training <- subset(training, select=-c(MARSUPWT)) # "instance weight" not used
training <- subset(training, select=-c(GRINST,HHDFMX,INCOME,ADTIND,ADTOCC))
training <- data.modificationOfCountryOfBirth(training)
training <- data.modificationBinaryVariable(training)

# one-hot encoding
dmy <- dummyVars(" ~ .", data = training)
training <- data.frame(predict(dmy, newdata = training))
training <- training[, !colnames(training) %in% colnames(training)[which(grepl( "Not.in.universe", colnames(training)))]]


# Cross-Validation to estimate the error rate
# Randomly shuffle the training data
training <- training[sample(nrow(training)),]

# Create 10 equally size folds
folds <- cut(seq(1,nrow(training)), breaks=10, labels=FALSE)
folds

err <- matrix(nrow=10, ncol=1)

#Perform 10-fold cross validation
for(i in 1:10){
  print(i)
  testIndexes <- which(folds==i,arr.ind = TRUE)
  
  # create the training and test sets
  Xtest <- training[testIndexes,]
  ztest <- z[testIndexes]
  Xtrain <- training[-testIndexes,]
  ztrain <- z[-testIndexes]

  # learn the model with the training dataset
  logistic2 <- cv.glmnet(x=as.matrix(Xtrain), y=ztrain, family="binomial", alpha=1, nfolds=5)
  # prediction for the test dataset, use of lambda.min as lambda
  pred <- as.factor(predict(logistic2, 
                            as.matrix(Xtest), 
                            s = "lambda.min",
                            type = "class"))
  # computation of the test error rate
  err[i] <- sum(pred != ztest)/length(ztest)
  print(err[i])
}

IC <- function(moy,var,n,alpha) {
  student <- qt(1-alpha/2, df = n-1)
  IC <- c(moy - student*sqrt(var/n), moy + student*sqrt(var/n))
  IC
}

# computation of the mean and the variance of the 10 test error rates
err_mean <- mean(err)
var <- apply(err,1, function(x) ((x-err_mean)^2))
var <- 1/(10-1)*apply(matrix(var),2,sum)
IC <- IC(err_mean,var,10,0.05) # confidence interval at 95%
IC

# scaling the training set and get the coefficients
training <- scale(training)

logistic <- cv.glmnet(x=as.matrix(training), y=z, family="binomial", alpha=1, nfolds=5)
coef(logistic, s= "lambda.min")
plot(logistic, xvar="lambda")