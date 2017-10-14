setwd("C:/Users/Bénédicte/Documents/UTC-GI/GI04/SY09/TP4")
source("Scripts/prob.log.r")
source("Scripts/prob.log2.r")

#donn_40 <- read.csv("Données/Synth1-40.csv")

# A. données 1-40
#X40 <- as.matrix(donn_40[,1:2])
#z40 <- as.matrix(donn_40[,3])

log.app <- function(Xapp, zapp, intr, epsi)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]

	Xapp <- as.matrix(Xapp)

	if (intr == T)
	{
		Xapp <- cbind(rep(1,n),Xapp)
		p <- p + 1
	}

	targ <- matrix(as.numeric(zapp),nrow=n)
	targ[which(targ==2),] <- 0
	tXap <- t(Xapp)
	beta <- matrix(0,nrow=p,ncol=1)

	conv <- F
	iter <- 0
	while (conv == F)
	{
		iter <- iter + 1
		bold <- beta
		
		prob <- exp(Xapp %*% beta) / (1 + exp(Xapp %*% beta))
		MatW <- diag(prob[,1])

		beta <- beta - solve(-t(Xapp) %*% MatW %*% Xapp ) %*% (t(Xapp) %*% (targ - prob))
		if (norm(beta-bold)<epsi)
		{
		  conv <- T
		}
	}
	prob <- postprob(beta, Xapp)
	out <- NULL
	out$beta <- beta
	out$iter <- iter
	calcul <- matrix(0,nrow=n,ncol=1)
  calcul<-apply(targ,1, 
                function(targ) targ%*%t(beta)%*%t(Xapp)-log(1+exp(t(beta)%*%t(Xapp))))
  out$logL<-sum(calcul)
	out
}

log.val <- function(beta, Xtst)
{
	m <- dim(Xtst)[1]
	p <- dim(beta)[1]
	pX <- dim(Xtst)[2]

	Xtst <- as.matrix(Xtst)

	if (pX == (p-1))
	{
		Xtst  <- cbind(rep(1,m),Xtst)
	}

	prob <- postprob(beta,Xtst)
	pred <- max.col(prob)

	out <- NULL
	out$prob <- prob
	out$pred <- pred

	return(out)
}

postprob <- function(beta, X)
{
	X <- as.matrix(X)
	calcul <- matrix(0,nrow=dim(X)[1],ncol=2)
	calcul[,1] <- exp(t(beta)%*%t(X))/(1+exp(t(beta)%*%t(X)))
	calcul[,2] <- 1/(1+exp(t(beta)%*%t(X)))
	return (calcul)
}

#logistique<-log.app(X40,z40,T,1*10^-5)
#val<-log.val(logistique$beta,X40)
#prob.log(logistique$beta, X40,z40,c(0.2,0.4,0.6,0.8))


# Regression logistique quadratique

#X <- X40
#X12 <- X[,1] * X[,2]
#X1_carre <- X[,1] * X[,1]
#X2_carre <- X[,2] * X[,2]

#X <- cbind(X[,1], X[,2], X12, X1_carre, X2_carre)

#logistique<-log.app(X,z40,T,1*10^-5)
#val<-log.val(logistique$beta,X)
#prob.log2(logistique$beta,X,z40,c(0.2,0.4,0.6,0.8))
