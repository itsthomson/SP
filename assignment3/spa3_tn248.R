rm(list=ls())

dmin2d <- function(n, m, s, xlo, xhi, ylo, yhi, plots=TRUE){
	## Models a spacial pattern according 
	## to the following parameters:
	##
	## n: number of points to simulate
	## m: mean of Normal distribution
	## s: s.d. of Normal distribution
	## xlo, xhi: possible range of X values.
	## ylo, yhi: possible range of Y values.
	## plots: bool for whether to create plots
	## (I added this so part II would run faster)

	## first point initialisation
	thegoodones <- c(runif(1, xlo, xhi),runif(1,ylo,yhi)) 

	while(length(thegoodones) < n * 2){
	  trialpoint <- c(runif(1, xlo, xhi),runif(1,ylo,yhi))
	  exclusionmin <- max(0, rnorm(1,m,s))
	  if(min(dist(rbind(thegoodones,trialpoint))) > exclusionmin)
	     {thegoodones <- rbind(thegoodones,trialpoint)}
	}

	if(plots==TRUE){
	plot(c(xlo-round(xlo*.05),xhi), c(ylo-round(xlo*.05), yhi), 
		type = "n", xlab="", ylab="",main=paste("dmin(",
		paste(n,m,s,xlo,ylo,xhi,yhi,sep=", "),")",sep=""))
	rect(xlo,ylo,xhi,yhi,lty=2)
	points(thegoodones[,1],thegoodones[,2],pch=19)
	  }

	rownames(thegoodones) <- NULL
	return(thegoodones)
	}

regind <- function(pointsdata){
	## Takes a dataframe of points and calculates
	## the regularity index.
	##
	## pointsdata: a two-column df of points

	mins <- NULL

	distmat <- as.matrix(dist(pointsdata))
	diag(distmat) <- Inf
	for(i in 1:dim(pointsdata)[1]){
	mins <- c(mins,min(distmat[,i]))
		}

	return(mean(mins)/sd(mins))
}


res <- dmin2d(200,30,5,200,1000,100,900)

theoretical <- replicate(1000,regind(dmin2d(200,0,0,0,1000,0,1000,FALSE)))
