setwd("C:/Users/Bénédicte/Documents/Dataiku")

# Load the data into a variable
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


# Summary
summary(training)

# Continuous variables
# To be improved ! Remove extreme points (to many points equal to 0)
boxplot(training$CAPGAIN[which(training$CAPGAIN != 0)])
length(training$CAPGAIN[which(training$CAPGAIN != 0)])/length(training$CAPGAIN)

# Male/Female V13
sex <- barplot(prop.table(table(training$ASEX)), main = "Sex of the population", 
        names.arg = levels(training$ASEX), ylim = c(0,0.60),
        col = c("sienna2","cadetblue3"))
percentage <- paste(apply(prop.table(table(training$ASEX)), 1, function(x) {round(x*100,2)}), "%")
text(x = sex, y = prop.table(table(training$ASEX)), 
     label = percentage, 
     pos = 3, cex = 1)


# Nominal variables
png(file = "Charts/V8.jpg")
pie(table(training$AMARITL), main="Variable AMARITL",
    labels=levels(training$AMARITL),col=rainbow(length(levels(training$AMARITL))))
dev.off()

pie(table(training$ARACE), main="Variable ARACE",
    labels=levels(training$ARACE),col=topo.colors(length(levels(training$ARACE))))

plot(training$ARACE)

barplot(table(training$AMJIND),names.arg=levels(training$AMJIND))
barplot(table(training$ACLSWKR), cex.names = 0.8, cex.axis = 0.8,
        main = "Variable AMJIND",
        names.arg=sapply(levels(training$ACLSWKR),FUN = function(x) {paste(strwrap(x, width = 8), collapse = "\n")}),las=2)

pie(table(training$AHGA), cex = 0.5,
        main = "Variable AHGA", col = rainbow(length(levels(training$AHGA)))
    )

summary(training$AHSCOL)

hist( training$AAGE, col = grey(0.9), border = grey(0.2),
      main = paste("Variable AAGE"),
      xlab = "Age [year]",
      ylab = "Population", las = 3, xlim = c(0,100), ylim = c(0, 40000),
      labels = T,
      breaks = seq(from = 0, to = 100, length = 11))

hist( training$AAGE, col = grey(0.9), border = grey(0.2),
      main = paste("Age of the data set"),
      xlab = "Age [year]",
      ylab = "Density", proba = T, xlim = c(0,100), ylim = c(0,0.020))

boxplot(training$AAGE, main = paste("Variable AAGE - Boxplot"), ylab = "Age [year]") # age data

# Weeks per year
hist( training$WKSWORK, col = grey(0.9), border = grey(0.2),
      main = paste("Variable WKSWORK"),
      xlab = "Number of weeks", las = 3,
      labels = F)
(length(training$WKSWORK[which(training$CAPGAIN == 0)])+length(training$WKSWORK[which(training$CAPGAIN == 52)]))/length(training$WKSWORK)*100


# Country of birth
# -> modifify the variable : USA or else

# Missing data
# ? = missing data

NIU <- matrix(0, 1, length(training))
questionMark <- matrix(0, 1, length(training))

for (i in 1:length(training)) {
  NIU[i] <- sum(training[,i] == " Not in universe") + sum(training[,i] == " Not in universe or children" )
  questionMark[i] <- sum(training[,i] == " ?")
}
