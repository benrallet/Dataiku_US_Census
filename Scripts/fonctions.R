setwd("C:/Users/Bénédicte/Documents/UTC-GI/GI04/SY09/TP4")
require(tree)
source("scripts/anadisc.r")
source("scripts/logistic.r")
source("scripts/separ1.r")

IC <- function(moy,var,n,alpha) {
  student <- qt(1-alpha/2, df = n-1)
  IC <- c(moy - student*sqrt(var/n), moy + student*sqrt(var/n))
  IC
}

# Fonctions d'analyse discriminante

# Analyse discriminante linéaire
adl.err <- function(X, z, N)  
{
  res <- NULL
  res$err <- matrix(nrow=N, ncol=1)
  
  res$matrice_confusion <- matrix(0, nrow=2,ncol=2)
  
  for (i in 1:N) {
    donn.sep <- separ1(X, z) # separation des données 
    Xapp <- donn.sep$Xapp
    zapp <- donn.sep$zapp
    Xtst <- donn.sep$Xtst
    ztst <- donn.sep$ztst
    
    param <- adl.app(Xapp,zapp)
    out <- ad.val(param,Xtst)
    
    mc <- matrix(0, nrow=2,ncol=2)
    mc[1,1] <- sum(out$pred[which(ztst==1)] == ztst[which(ztst==1)])
    mc[1,2] <- sum(out$pred[which(ztst==1)] != ztst[which(ztst==1)])
    mc[2,1] <- sum(out$pred[which(ztst==2)] != ztst[which(ztst==2)])
    mc[2,2] <- sum(out$pred[which(ztst==2)] == ztst[which(ztst==2)])
    
    print(length(ztst))
    
    res$matrice_confusion <- res$matrice_confusion + mc
    
    res$err[i] <- sum(out$pred != ztst)/length(ztst)
  }
  res$err_mean <- mean(res$err)
  var <- apply(res$err,1, function(x) ((x-res$err_mean)^2))
  res$var <- 1/(N-1)*apply(matrix(var),2,sum)
  res$IC <- IC(res$err_mean,res$var, N,0.05)
  res$matrice_confusion <- res$matrice_confusion/N
  res
}

# Analyse discriminante quadratique
adq.err <- function(X, z, N)  
{
  res <- NULL
  res$err <- matrix(nrow=N, ncol=1)
  res$matrice_confusion <- matrix(0, nrow=2,ncol=2)
  for (i in 1:N) {
    print(i)
    out <- NULL
    donn.sep <- separ1(X, z) # separation des données 
    Xapp <- donn.sep$Xapp
    zapp <- donn.sep$zapp
    Xtst <- donn.sep$Xtst
    ztst <- donn.sep$ztst
    
    param <- adq.app(Xapp,zapp)
    out <- ad.val(param,Xtst)
    #try(out <- ad.val(param,Xtst),silent=TRUE)
    #if(is.null(out)) {
     # res$err[i] <- -1
    #}
    #else {
      res$err[i] <- sum(out$pred != ztst)/length(ztst)
    #}
      
      mc <- matrix(0, nrow=2,ncol=2)
      mc[1,1] <- sum(out$pred[which(ztst==1)] == ztst[which(ztst==1)])
      mc[1,2] <- sum(out$pred[which(ztst==1)] != ztst[which(ztst==1)])
      mc[2,1] <- sum(out$pred[which(ztst==2)] != ztst[which(ztst==2)])
      mc[2,2] <- sum(out$pred[which(ztst==2)] == ztst[which(ztst==2)])
      
      res$matrice_confusion <- res$matrice_confusion + mc
  }
 # res$err <- matrix(res$err[which(res$err!=-1)])
  #N <- length(res$err)
  res$err_mean <- mean(res$err)
  var <- apply(res$err,1, function(x) ((x-res$err_mean)^2))
  res$var <- 1/(N-1)*apply(matrix(var),2,sum)
  res$IC <- IC(res$err_mean,res$var, N,0.05)
  res$matrice_confusion <- res$matrice_confusion/N
  res
}

# Classifieur bayesien naïf
nba.err <- function(X, z, N)  
{
  res <- NULL
  res$err <- matrix(nrow=N, ncol=1)
  res$matrice_confusion <- matrix(0, nrow=2,ncol=2)
  for (i in 1:N) {
    out <- NULL
    print(i)
    donn.sep <- separ1(X, z) # separation des données 
    Xapp <- donn.sep$Xapp
    zapp <- donn.sep$zapp
    Xtst <- donn.sep$Xtst
    ztst <- donn.sep$ztst
    
    param <- nba.app(Xapp,zapp)
    try(out <- ad.val(param,Xtst),silent=TRUE)
    if(is.null(out)) {
      res$err[i] <- -1
    }
    else {
      res$err[i] <- sum(out$pred != ztst)/length(ztst)
    }
    
    mc <- matrix(0, nrow=2,ncol=2)
    mc[1,1] <- sum(out$pred[which(ztst==1)] == ztst[which(ztst==1)])
    mc[1,2] <- sum(out$pred[which(ztst==1)] != ztst[which(ztst==1)])
    mc[2,1] <- sum(out$pred[which(ztst==2)] != ztst[which(ztst==2)])
    mc[2,2] <- sum(out$pred[which(ztst==2)] == ztst[which(ztst==2)])
    
    res$matrice_confusion <- res$matrice_confusion + mc
  }
  res$err <- matrix(res$err[which(res$err!=-1)])
  N <- length(res$err)
  res$err_mean <- mean(res$err)
  var <- apply(res$err,1, function(x) ((x-res$err_mean)^2))
  res$var <- 1/(N-1)*apply(matrix(var),2,sum)
  res$IC <- IC(res$err_mean,res$var, N,0.05)
  res$matrice_confusion <- res$matrice_confusion/N
  res
}

# Régression logistique
log.err <- function(X, z, N)  
{
  res <- NULL
  res$err <- matrix(nrow=N, ncol=1)
  res$matrice_confusion <- matrix(0, nrow=2,ncol=2)
  for (i in 1:N) {
    print(i)
    donn.sep <- separ1(X, z) # separation des données 
    Xapp <- donn.sep$Xapp
    zapp <- donn.sep$zapp
    Xtst <- donn.sep$Xtst
    ztst <- donn.sep$ztst
    
    param <-log.app(Xapp,zapp,T,10^-5)
    out <-log.val(param$beta,Xtst)
    
    res$err[i] <- sum(out$pred != ztst)/length(ztst)
    
    mc <- matrix(0, nrow=2,ncol=2)
    mc[1,1] <- sum(out$pred[which(ztst==1)] == ztst[which(ztst==1)])
    mc[1,2] <- sum(out$pred[which(ztst==1)] != ztst[which(ztst==1)])
    mc[2,1] <- sum(out$pred[which(ztst==2)] != ztst[which(ztst==2)])
    mc[2,2] <- sum(out$pred[which(ztst==2)] == ztst[which(ztst==2)])
    
    res$matrice_confusion <- res$matrice_confusion + mc
  }
  res$err_mean <- mean(res$err)
  var <- apply(res$err,1, function(x) ((x-res$err_mean)^2))
  res$var <- 1/(N-1)*apply(matrix(var),2,sum)
  res$IC <- IC(res$err_mean,res$var, N,0.05)
  res$matrice_confusion <- res$matrice_confusion/N
  res
}

# Régression logistique quadratique
log2.err <- function(X, z, N)  
{
  res <- NULL
  res$err <- matrix(nrow=N, ncol=1)
  res$matrice_confusion <- matrix(0, nrow=2,ncol=2)
  for (i in 1:N) {
    print(i)
    donn.sep <- separ1(X, z) # separation des données 
    Xapp <- donn.sep$Xapp
    zapp <- donn.sep$zapp
    Xtst <- donn.sep$Xtst
    ztst <- donn.sep$ztst
    
    Xapp2 <- Xapp
    Xtst2 <- Xtst
    
    for (p in 1:(dim(Xapp)[2]-1))
    {
      for (q in (p+1):dim(Xapp)[2])
      {
        Xapp2 <- cbind(Xapp2, Xapp[,p]*Xapp[,q])
        Xtst2 <- cbind(Xtst2, Xtst[,p]*Xtst[,q])
      }
    }
    for (p in 1:(dim(Xapp)[2]-1))
    {
      Xapp2 <- cbind(Xapp2, Xapp[,p]^2)
      Xtst2 <- cbind(Xtst2, Xtst[,p]^2)
    }
    
    param <-log.app(Xapp2,zapp,T,10^-5)
    out <-log.val(param$beta,Xtst2)
    
    res$err[i] <- sum(out$pred != ztst)/length(ztst)
    
    mc <- matrix(0, nrow=2,ncol=2)
    mc[1,1] <- sum(out$pred[which(ztst==1)] == ztst[which(ztst==1)])
    mc[1,2] <- sum(out$pred[which(ztst==1)] != ztst[which(ztst==1)])
    mc[2,1] <- sum(out$pred[which(ztst==2)] != ztst[which(ztst==2)])
    mc[2,2] <- sum(out$pred[which(ztst==2)] == ztst[which(ztst==2)])
    
    res$matrice_confusion <- res$matrice_confusion + mc
  }
  res$err_mean <- mean(res$err)
  var <- apply(res$err,1, function(x) ((x-res$err_mean)^2))
  res$var <- 1/(N-1)*apply(matrix(var),2,sum)
  #res$IC <- IC(res$err_mean,res$var, N,0.05)
  res$matrice_confusion <- res$matrice_confusion/N
  res
}

tree.err <- function(X,z,N)
{
  res <- NULL
  res$err <- matrix(nrow=N, ncol=1)
  res$matrice_confusion <- matrix(0, nrow=2,ncol=2)
  for (i in 1:N) {
    print(i)
    donn.sep <- separ1(X, z) # separation des données 
    Xapp <- as.data.frame(donn.sep$Xapp)
    zapp <- factor(donn.sep$zapp)
    Xtst <- donn.sep$Xtst
    ztst <- donn.sep$ztst
    
    control_tree <- tree.control(nobs=dim(Xapp)[1],mindev = 0.0001) # arbre complet
    tr <- tree(zapp~., data.frame(Xapp), control = control_tree) # zapp est un facteur
    validation <- cv.tree(tr, FUN = prune.misclass) # séquence d'arbres emboités + estimation d'erreur par validation croisée
    #prune.misclass correspond à l'élagage élagage
    prob <- predict(tr, Xtst) # classe un jeu de données au moyen de l'arbre
    val <- as.numeric(rownames(prob))
    prob <- prob[order(val),]
    pred <- as.matrix(max.col(prob))
    res$err[i] <- sum(pred != ztst)/length(ztst)
    
    mc <- matrix(0, nrow=2,ncol=2)
    mc[1,1] <- sum(out$pred == ztst && ztst == 1)
    mc[1,2] <- sum(out$pred != ztst && ztst == 1)
    mc[2,1] <- sum(out$pred != ztst && ztst == 2)
    mc[2,2] <- sum(out$pred == ztst && ztst == 2)
    
    res$matrice_confusion <- res$matrice_confusion + mc
  }
  res$err_mean <- mean(res$err)
  var <- apply(res$err,1, function(x) ((x-res$err_mean)^2))
  res$var <- 1/(N-1)*apply(matrix(var),2,sum)
  res$IC <- IC(res$err_mean,res$var, N,0.05)
  res$matrice_confusion <- res$matrice_confusion/N
  res
}

