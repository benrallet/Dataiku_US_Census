setwd("C:/Users/Bénédicte/Documents/Dataiku")
source("Scripts/fonctions.r")

# Load the data into a variable
training <- read.csv("Data/census_income_learn.csv", header=F)

X <- as.data.frame(testFrame2[,1:100])

z <- as.numeric(training[,41])
z <- apply(as.data.frame(z), 1, function(x) {x-1})

# Logistic regression
fm <- glm(z ~ ., 
          family=binomial, data=X)
p <- round(predict(fm,type="response"))

sum(p != z)/length(z)

z <- as.matrix(z)

# confusion matrix
mc <- matrix(0, nrow=2,ncol=2)
mc[1,1] <- sum(p[which(z==0)] == z[which(z==0)])
mc[1,2] <- sum(p[which(z==0)] != z[which(z==0)])
mc[2,1] <- sum(p[which(z==1)] != z[which(z==1)])
mc[2,2] <- sum(p[which(z==1)] == z[which(z==1)])

X <- as.data.frame(X)
z <- as.numeric(training$V42)
z <- apply(as.data.frame(z), 1, function(x) {x-1})


res <- NULL
N <- 20
res$err_tst <- matrix(nrow=N, ncol=1)
res$err_app <- matrix(nrow=N, ncol=1)

for (i in 1:N) {
  print(i)
  donn.sep <- separ1(X, z) # separation des données 
  Xapp <- as.data.frame(donn.sep$Xapp)
  zapp <- donn.sep$zapp
  Xtst <- as.data.frame(donn.sep$Xtst)
  ztst <- donn.sep$ztst
  
  # Logistic regression
  V1 <- Xapp$V1
  V2 <- Xapp$V2
  V3 <- Xapp$V3
  V4 <- Xapp$V4
  V5 <- Xapp$V5
  V6 <- Xapp$V6
  fm <- glm(zapp ~ V1 + V2 + V3 + V4 + V5 + V6, 
            family=binomial, data=Xapp)
  papp <- round(predict(fm,type="response"))
  res$app[i] <- sum(papp != zapp)/length(zapp)
  
  ptst <- round(predict(fm,Xtst,type="response"))
  res$tst[i] <- sum(ptst != ztst)/length(ztst)
}

res$app_mean <- mean(res$app)
res$tst_mean <- mean(res$tst)

test <- read.csv("Data/census_income_test.csv", header=F)

X_test <- cbind(test$V1, test$V17)
X_test <- cbind(X_test, test$V18)
X_test <- cbind(X_test, test$V19)
X_test <- cbind(X_test, test$V31)
X_test <- cbind(X_test, test$V40)
X_test <- cbind(X_test, test$V6)

X_test<- as.data.frame(X_test)

z <- as.numeric(test$V42)
z <- apply(as.data.frame(z), 1, function(x) {x-1})

p <- round(predict(fm, X_test, type="response"))
error <- sum(p != z)/length(test$V42)

# glmnet
library(glmnet)
logistic <- glmnet(x=as.matrix(testFrame2), y=z, family="binomial", alpha=1)
predict(logistic, as.matrix(testFrame2))
logistic2 <- cv.glmnet(x=as.matrix(testFrame2), y=z, family="binomial", alpha=1, nfolds=5)

pred <- as.factor(predict(logistic2, 
                          as.matrix(testFrame2), 
                          s = "lambda.min",
                          type = "class"))
error <- sum(pred != z)/length(z)

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

predtst <- as.factor(predict(logistic2, 
                          as.matrix(test), 
                          s = "lambda.min",
                          type = "class"))
error <- sum(predtst != ztst)/length(ztst)
error
