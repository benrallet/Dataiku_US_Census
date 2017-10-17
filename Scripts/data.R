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

z <- training[,"INCOME"]

# The attribute "instance weight" should *not* be used in the 
#classifiers, so it is set to "ignore" in this file
training <- subset(training, select=-c(MARSUPWT))
training <- subset(training, select=-c(GRINST,HHDFMX,INCOME))

data.modificationOfCountryOfBirth <- function(X) {
  for (col in c("PEFNTVTY", "PEMNTVTY", "PENATVTY")) {
    X[[col]] <- apply(as.data.frame(X[[col]]), 1, function(x) {if (x == " United-States") x <- 1 else x <- 0})
  }
  X
}
data.modificationBinaryVariable <- function(X) {
  X[["ASEX"]] <- apply(as.data.frame(X[["ASEX"]]), 1, function(x) {if (x == " Female") x <- 1 else x <- 0})
  X[["YEAR"]] <- apply(as.data.frame(X[["YEAR"]]), 1, function(x) {if (x == 95) x <- 1 else x <- 0})
  X
}
data.modificationYesNoQuestions <- function(X) {
  for (col in c("AUNMEM", "MIGSAME", "MIGSUN", "VETQVA")) {
    X[[col]] <- apply(as.data.frame(X[[col]]), 1, function(x) {if (x == " Yes") x <- 1 else x <- 0})
  }
  X
}

training <- data.modificationOfCountryOfBirth(training)
training <- data.modificationBinaryVariable(training)
#training <- data.modificationYesNoQuestions(training)

library(caret)
dmy <- dummyVars(" ~ .", data = training)
testFrame2 <- data.frame(predict(dmy, newdata = training))

testFrame2 <- testFrame2[, !colnames(testFrame2) %in% colnames(testFrame2)[which(grepl( "Not.in.universe", colnames(testFrame2)))]]


