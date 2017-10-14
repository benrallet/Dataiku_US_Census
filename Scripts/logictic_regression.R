setwd("C:/Users/Bénédicte/Documents/Dataiku")
source("Scripts/fonctions.r")

# Load the data into a variable
training <- read.csv("Data/census_income_learn.csv", header=F)

# Construction of the data set with new variables
X <- cbind(training$V1, training$V17)
X <- cbind(X, training$V18)
X <- cbind(X, training$V19)
X <- cbind(X, training$V31)
X <- cbind(X, training$V40)
X <- cbind(X, training$V6)

X <- as.data.frame(X)

z <- as.numeric(training$V42)
z <- apply(as.data.frame(z), 1, function(x) {x-1})

# Logistic regression
fm <- glm(z ~ training$V1 + training$V6 +training$V17 + training$V17 + training$V18 + training$V19 + training$V31 + training$V40, 
          family=binomial, data=training)
p <- round(predict(fm,type="response"))

sum(p != z)/length(z)



X <- as.data.frame(X)
z <- as.numeric(training$V42)
z <- apply(as.data.frame(z), 1, function(x) {x-1})


err <- NULL
N <- 100
res$err_tst <- matrix(nrow=N, ncol=1)
res$err_app <- matrix(nrow=N, ncol=1)

for (i in 1:N) {
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
  err$app[i] <- sum(papp != zapp)/length(zapp)
  
  ptst <- round(predict(fm,Xtst,type="response"))
  err$tst[i] <- sum(ptst != ztst)/length(ztst)
}

err$app_mean <- mean(err$app)
err$tst_mean <- mean(err$tst)
