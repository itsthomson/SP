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
		paste(n,m,s,xlo,xhi,ylo,yhi,sep=", "),")",sep=""))
	rect(xlo,ylo,xhi,yhi,lty=2)
	points(thegoodones[,1],thegoodones[,2],pch=19)
	  }

	rownames(thegoodones) <- NULL
	return(thegoodones)
	}

regind <- function(pointsdata,hist=TRUE){
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
		
	if(hist==TRUE){
		hist(mins,xlab="Distance")
		}
	return(mean(mins)/sd(mins))
}

# PART I
res <- dmin2d(200,30,5,200,1000,100,900)

# PART II

# square
theoretical <- replicate(1000,regind(dmin2d(200,0,0,0,1000,0,1000,FALSE)))
theoretical <- theoretical[rev(order(theoretical))]
theoretical[50]

# rectangle
theoreticalrect <- replicate(1000,regind(dmin2d(200,0,0,0,500,0,2000,FALSE)))
theoreticalrect <- theoreticalrect[rev(order(theoreticalrect))]
theoreticalrect[50]

# half points
theoreticalhalf <- replicate(1000,regind(dmin2d(100,0,0,0,1000,0,1000,FALSE)))
theoreticalhalf <- theoreticalhalf[rev(order(theoreticalhalf))]
theoreticalhalf[50]

theoreticalhalf2 <- replicate(1000,regind(dmin2d(50,0,0,0,1000,0,1000,FALSE)))
theoreticalhalf2 <- theoreticalhalf2[rev(order(theoreticalhalf2))]
theoreticalhalf2[50]

# more points
theoreticalmore <- replicate(1000,regind(dmin2d(3,0,0,0,1000,0,1000,FALSE)))
theoreticalmore <- theoreticalmore[rev(order(theoreticalmore))]
theoreticalmore[50]

# PART III

## load the real data
real <- read.table("spa3_real.dat")
plot(c(0,400), c(0,400), 
		type = "n", xlab="", ylab="",main="Real Data")
rect(0,0,400,400,lty=2)	
points(real[,1],real[,2],pch=19)

fitfinder <- function(m,s,real){
	## calculates u-score for a given set of m,s parameters.
	## n=238, xlo,ylo = 0, xhi,yhi = 400
	## real= real dataset
	uscore <- NULL
	ffsimulated <- replicate(99,regind(dmin2d(150,m,s,0,400,0,400,FALSE)))
	rri <- regind(real)
	ris <- c(rri,ffsimulated)
	
	for(i in 1:100){
		currentri <- ris[i]
		ristemp <- ris[-i]
		uscore[i] <- abs(currentri - sum(ristemp)/99)
	}
	return(uscore)
}
