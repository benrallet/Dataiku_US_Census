setwd("C:/Users/Bénédicte/Documents/Dataiku")
source("Scripts/fonctions.r")

# Load the data into a variable
training <- read.csv("Data/census_income_learn.csv", header=F)

# Summary
summary(training)

# Continuous variables
# To be improved ! Remove extreme points (to many points equal to 0)
boxplot(training$CAPGAIN[which(training$CAPGAIN != 0)])
length(training$CAPGAIN[which(training$CAPGAIN != 0)])/length(training$CAPGAIN)

# Male/Female V13
sex <- barplot(prop.table(table(training$ASEX)), main = "Sex of the population", 
        names.arg = levels(training$ASEX), ylim = c(0,0.60))
percentage <- paste(apply(prop.table(table(training$ASEX)), 1, function(x) {round(x*100,2)}), "%")
text(x = sex, y = prop.table(table(training$ASEX)), 
     label = percentage, 
     pos = 3, cex = 1, col = "red")


# Nominal variables
png(file = "Charts/V8.jpg")
pie(table(training$AMARITL), main="Variable v8",
    labels=levels(training$AMARITL),col=rainbow(length(levels(training$AMARITL))))
dev.off()

pie(table(training$ARACE), main="Variable v11",
    labels=levels(training$ARACE),col=rainbow(length(levels(training$ARACE))))

plot(training$ARACE)

barplot(table(training$AMJIND),names.arg=levels(training$AMJIND))

hist( training$AAGE, col = grey(0.9), border = grey(0.2),
      main = paste("Age of the data set"),
      xlab = "Age [year]",
      ylab = "Population", las = 3, xlim = c(0,100), ylim = c(0, 40000),
      labels = T,
      breaks = seq(from = 0, to = 100, length = 11))

hist( training$AAGE, col = grey(0.9), border = grey(0.2),
      main = paste("Age of the data set"),
      xlab = "Age [year]",
      ylab = "Density", proba = T, xlim = c(0,100), ylim = c(0,0.020))

boxplot(training$AAGE) # age data

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
