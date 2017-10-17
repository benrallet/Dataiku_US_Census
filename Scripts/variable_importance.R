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

# Age 
boxplot(training$AAGE~z, col=c("sienna2","cadetblue3"), xlab="Income", ylab="Age")

# Weeks worked
boxplot(training$WKSWORK~z, col=c("sienna2","cadetblue3"), xlab="Income", ylab="Sex")
summary(training$WKSWORK[which(as.numeric(z)==1)])
summary(training$WKSWORK[which(as.numeric(z)==2)])

# sex
table(training$ASEX,z)

# working code 
table(training$ADTOCC,z)
