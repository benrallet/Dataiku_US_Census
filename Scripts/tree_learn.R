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

z <- as.factor(training[,39])

# Fit the model with all the training data
control_tree <- tree.control(nobs=dim(training[,1:38])[1],mindev = 0.0001) # entire tree
tr <- tree(z ~ ., data.frame(training[,1:38]), control = control_tree) # zapp has to be factor

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
err <- sum(pred != ztest)/length(ztest)

# confusion matrix
mc <- matrix(0, nrow=2,ncol=2)
mc[1,1] <- sum(pred[which(ztest==1)] == ztest[which(ztest==1)])
mc[1,2] <- sum(pred[which(ztest==1)] != ztest[which(ztest==1)])
mc[2,1] <- sum(pred[which(ztest==2)] != ztest[which(ztest==2)])
mc[2,2] <- sum(pred[which(ztest==2)] == ztest[which(ztest==2)])
mc
