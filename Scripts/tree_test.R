# Application of the model to the test file
setwd("C:/Users/Bénédicte/Documents/Dataiku")
set.seed(100)
source("Scripts/data.R")
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
z <- training[,"INCOME"]

# Pre-processing
# The attribute "instance weight" should *not* be used in the 
# classifiers, so it is set to "ignore" in this file
training <- subset(training, select=-c(MARSUPWT))

training <- data.modificationOfCountryOfBirth(training)
training <- subset(training, select=-c(GRINST,HHDFMX,INCOME))
training <- subset(training, select=-c(ADTIND,ADTOCC))

# Fit the model with all the training data
control_tree <- tree.control(nobs=dim(training)[1],mindev = 0.0001) # entire tree
tr <- tree(as.factor(z) ~ ., data.frame(training), control = control_tree, wts = TRUE) # zapp has to be factor

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

ztest <- test[,"INCOME"]

# Same pre-processing than for the training data
test <- data.modificationOfCountryOfBirth(test)
test <- subset(test, select=-c(GRINST,HHDFMX,INCOME))
test <- subset(test, select=-c(ADTIND,ADTOCC))

# prediction
pred <- predict(tr, test, type = "class") # classe un jeu de données au moyen de l'arbre
err <- sum(pred != ztest)/length(ztest) # test error rate

table(ztest,pred) # confusion matrix

# confusion matrix
confusionMatrix(ztest,pred)