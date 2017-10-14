setwd("C:/Users/Bénédicte/Documents/UTC-GI/GI04/SY09/TP4")
source("scripts/mvdnorm.r")
source("scripts/prob.ad.r")

#donn_40 <- read.csv("Données/Synth1-40.csv")

# A. données 1-40
#X40 <- as.matrix(donn_40[,1:2])
#z40 <- as.matrix(donn_40[,3])

adq.app <- function(Xapp, zapp)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]
	g <- max(unique(zapp))

	param <- NULL
	param$MCov <- array(0, c(p,p,g))
	param$mean <- array(0, c(g,p))
	param$prop <- rep(0, g)

	for (k in 1:g)
	{
		indk <- which(zapp==k)
		Xk <- as.matrix(Xapp[indk,])

		param$MCov[,,k] <- var(Xk)
		param$mean[k,] <- apply(Xk,2,mean)
		param$prop[k] <- nrow(Xk)/n
	}
	param
}

adl.app <- function(Xapp, zapp)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]
	g <- max(unique(zapp))

	param <- NULL
	MCov <- array(0, c(p,p))
	param$MCov <- array(0, c(p,p,g))
	param$mean <- array(0, c(g,p))
	param$prop <- rep(0, g)

	for (k in 1:g)
	{
		indk <- which(zapp==k)
		Xk <- as.matrix(Xapp[indk,])

		MCov <- MCov + nrow(Xk)*var(Xk)
		param$mean[k,] <- apply(Xk,2,mean)
		param$prop[k] <- nrow(Xk)/n
	}
	MCov <- MCov/n
	for (k in 1:g)
	{
		param$MCov[,,k] <- MCov
	}

	param
}

nba.app <- function(Xapp, zapp)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]
	g <- max(unique(zapp))

	param <- NULL
	param$MCov <- array(0, c(p,p,g))
	param$mean <- array(0, c(g,p))
	param$prop <- rep(0, g)

	for (k in 1:g)
	{
		indk <- which(zapp==k)
		Xk <- as.matrix(Xapp[indk,])

		param$MCov[,,k] <- diag(diag(var(Xk)))
		param$mean[k,] <- apply(Xk,2,mean)
		param$prop[k] <- nrow(Xk)/n 
	}
	param
}

ad.val <- function(param, Xtst)
{
	n <- dim(Xtst)[1]
	p <- dim(Xtst)[2]
	g <- length(param$prop)

	out <- NULL

	prob <- matrix(0, nrow=n, ncol=g)
  denom <- matrix(0, ncol=2, nrow=n)
	
	for (k in 1:g)
	{
		prob[,k] <- mvdnorm(Xtst,param$mean[k,],param$MCov[,,k])
		denom[,1] <- denom[,1] + prob[,k]*param$prop[k]
	}
  denom[,2] <- denom[,1]
	prob <- prob%*%diag(param$prop)
	prob <- prob/denom
	#prob[which(is.nan(prob))] <- 0 
	pred <- max.col(prob)

	out$prob <- prob
	out$pred <- pred
	out$denom

	out
}

#adl_40 <- adl.app(X40,z40)
#adq_40 <- adq.app(X40,z40)
#nba_40 <- nba.app(X40,z40)
#prob.ad(adl_40,X40,z40,c(0.2,0.4,0.6,0.8))
#prob.ad(adq_40,X40,z40,c(0.2,0.4,0.6,0.8))
#prob.ad(nba_40,X40,z40,c(0.2,0.4,0.6,0.8))

