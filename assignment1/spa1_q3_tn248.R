# ##
# spa1_q3_tn248.R
# This script will simulate a simple model 
# for density-independent population
# growth with spatial variation, as per 
# Exercise 10 in "An introduction to R 
# for dynamic models in biology". 
# ##

# ##
# Created 18 Oct 2010
# Last updated 26 Oct 2010
# tn248@cam.ac.uk
# ##

# parameters, adjustable
L <- 20 # how many patches?
t.final <- 50 # how many iterations?
initsize <- 5 # what's our initial population in each patch?
d <- 0.1 # what's the dispersal rate? (important: it's 2*d)

# what and where are the growth rates?
lambda.one <- 1.2
one.locations <- 1:10 #c(1,3,5,7,9,11,13,15,17,19) # for maximum flexibility

lambda.two <- .9
two.locations <- 11:20 #c(2,4,6,8,10,12,14,16,18,20)

# initialize everything up
population = matrix(rep(0,L*t.final), t.final,L)
population[1,] = rep(initsize,L)

onegrowth <- function(t,j){
	geogrowth <- lambda.one * population[t,j]
return(geogrowth)
}

twogrowth <- function(t,j){
	geogrowth <- lambda.two * population[t,j]
return(geogrowth)
}

# This function will take N_j(t) and produce N_j(t+1)
reflecting <- function(t,j){#not entirely happy with this

if(j %in% one.locations)
	{
		type = "one"
		if (j==1){type= "leftendone"}
		if (j==L){type= "rightendone"}	
	}
if(j %in% two.locations)
	{
		type = "two"
		if (j==1){type= "leftendtwo"}
		if (j==L){type= "rightendtwo"}
	}

newpop <- switch(type, 
	leftendone = (1-d)*onegrowth(t,j) + d*onegrowth(t,j+1),
	leftendtwo = (1-d)*twogrowth(t,j) + d*twogrowth(t,j+1),
	one = (1 - 2*d)*onegrowth(t,j) + d*onegrowth(t,j-1) + 				d*onegrowth(t,j+1),
	two = (1 - 2*d)*twogrowth(t,j) + d*twogrowth(t,j-1) + 				d*twogrowth(t,j+1),
	rightendone = (1-d)*onegrowth(t,j) + d*onegrowth(t,j-1),
	rightendtwo = (1-d)*twogrowth(t,j) + d*twogrowth(t,j-1),
			)
return(newpop)
	}

# This will produce populations for t+1
for(i in 1:(t.final-1)){
population[i+1,]<- mapply(reflecting,rep(i,L),1:L)
}

par(mfrow=c(3,3))
for (i in floor(seq(1,L, length=9))){
plot(1:t.final,log(population[,i]), xlab="Time", ylab="log(Population)",type="l", main=paste("Patch ", i),ylim=c(0,log(max(population))))
}