# ##
# spa1_q2_tn248.R
# This script will simulate a game of squash.
# It will then simulate many games of squash, and plot
# some p.hat estimations.
# ##

# ##
# Created 27 Oct 2010
# Last updated 29 Oct 2010
# tn248@cam.ac.uk
# ##

library(spuRs)

# The following two functions were taken from the spuRs
# package, a companion to Scientific Programming Using R 
# by Jones, Maillardet, and Robinson.

status.test <- function(s.ftn){
	x.vec <- (-1):11
	y.vec <- (-1):11
	plot(x.vec, y.vec, type = "n", xlab="x",ylab="y")
	for(x in x.vec){
		for(y in y.vec){
			s <- s.ftn(x,y)
			if (s == "impossible") text(x,y,"X",col="red")
			else if (s == "unfinished") text(x,y, "?", col="blue")
			else if (s == "player 1 win") text(x,y,"1", col="green")
			else if (s == "player 2 win") text(x,y,"2", col="green")
			}
		}
	return(invisible(NULL))
}

play_game<- function(a,b){
	state <- c(0, 0, 1) # player 1 serves first
	while (status(state[1],state[2]) == "unfinished"){
		#show(state)
		state <- play_point(state, a, b)
		}
	if (status(state[1],state[2]) == "player 1 win"){
		return(TRUE)
	} else{
		return(FALSE)
	}
}

# The rest is mine.

status <- function(x,y){
	if (x < 0 | y < 0 | (x > 9 & x-y > 2) | (y > 9 & y-x >2)){
		return("impossible")}
	else if((x == 9 & x-y >= 2) | (x > 9 & x-y == 2)){
		return("player 1 win")}
	else if((y == 9 & y-x >= 2) | (y > 9 & y-x == 2)){
		return("player 2 win")}
	else{return("unfinished")}
	}

play_point <- function(state,a,b){
	# for my reference:
	# state = (x,y,z)
	# x is player 1's score
	# y is player 2's score
	# z indicates whose serve
	# a is probability 1 wins a point when 1 serves
	# b is probability 1 wins a point when 2 serves
	if (state[3]==1){
		odds <- a}
	else{odds <-b}
		
	# the game!
	point <- rbinom(1,1,odds)
	
	if (point == 1){
		if (state[3]==1) {state[1] <- state[1] + 1}
		else if (state[3]==2) {state[3] <- 1}
		}
	else if (point == 0){
		if (state[3]==1) {state[3] <- 2}
		else if (state[3]==2) {state[2] <- state[2] +1}
		}
	
	return(state)
	}

# The seed is my birthday, in American date form. 
# Set once and only once.
seed <- 110685
set.seed(seed)

# Estimating probability for different sample sizes
sample.size <- 12
probabilities <- rep(NA,sample.size)

# this will take a while
probabilities <- sapply(1:sample.size,function(x){sum(mapply(play_game,rep(.55,2^x),rep(.45,2^x)))/2^x})

plot(1:sample.size, probabilities,main="Estimated probability for different sample sizes", xlab="log(n)/log(2)", ylab="p_hat")

# Table of estimated p(a,b) values
resolution <- 250
p.hat.values <- cbind(rep(seq(.1,.9,by=.1),each=9),rep(seq(.1,.9,by=.1),9))

# This one-liner will create a table of p.hats by simulating 
# games for each pair in p.hat.values. Might take a while depending on
# the resolution.
p.hat.table <- matrix(mapply(function(x,y){sum(mapply(play_game,rep(x,resolution),rep(y,resolution)))/resolution},p.hat.values[,1],p.hat.values[,2]),9,9,byrow=TRUE)

play_game_length <- function(a,b){
	state <- c(0, 0, 1) # player 1 serves first
	count <- 0
	while (status(state[1],state[2]) == "unfinished"){
		#show(state)
		state <- play_point(state, a, b)
		count <- count + 1
		}
	if (status(state[1],state[2]) == "player 1 win"){
		return(count)
	} else{
		return(count)
	}
}

length.table <- matrix(mapply(function(x,y){sum(mapply(play_game_length,rep(x,resolution),rep(y,resolution)))/resolution},p.hat.values[,1],p.hat.values[,2]),9,9,byrow=TRUE)