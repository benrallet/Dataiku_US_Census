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

# Cap Gain
boxplot(training$CAPGAIN[which(training$CAPGAIN!=0)]~z[which(training$CAPGAIN!=0)], col=c("sienna3","cadetblue2"), xlab="Income", ylab="Capital Gains")

# Weeks worked
boxplot(training$WKSWORK~z, col=c("sienna2","cadetblue3"), xlab="Income", ylab="Number of weeks")
summary(training$WKSWORK[which(as.numeric(z)==1)])
barplot(training$WKSWORK[which(as.numeric(z)==1)])
summary(training$WKSWORK[which(as.numeric(z)==2)])
hist(training$WKSWORK[which(as.numeric(z)==2)],
     col = grey(0.9), border = grey(0.2),
     main = paste("Variable WKSWORK for $ +50000"),
     xlab = "Number of weeks", las = 3,
     labels = F)

# working code 
apply(table(training$AMJOCC,z),2,function(x) {x/sum(x)*100})

# filer status
apply(table(training$FILESTAT,z),2,function(x) {x/sum(x)*100})

# education level
apply(table(training$AHGA,z),2,function(x) {x/sum(x)*100})
