# don't submit this!

# This function takes two arguments, a number and a bet. If you want to bet 
# on red, the number is -1. If you want to bet on black, the number is -2.
# This exercise only assumes straight-up and color bets, so no splits, streets
# corners, sixes, trios, baskets, etc. The output is how much you've won (lost).

roulettespin <- function(number, bet){
	# The spin!
	winner <- sample(0:36,1)
	
	# In roulette, odd numbers are red and even are black in [1,10] and [19,28].
	# In [11,18] and [29,36], odd numbers are black, and even are red. 
	# How confusing! 
	
	red <- c(1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36)
	black <- c(2,4,6,8,10,11,13,15,17,20,22,24,26,28,29,31,33,35)
	green <- 0
	
	spinwin <- 0
	
	if(winner %in% red)
	winner[2] <- -1
	else if(winner %in% black)
	winner[2] <- -2
	else if(winner %in% green)
	winner[2] <- 0 #should never be used
	
	if(number < 0 & winner[2] == number){
			spinwin <- bet} # 1:1 odds 
	else if(number == winner[1]){
		spinwin <- bet * 35} # 35:1 odds
	else{
		spinwin <- -bet}
		
	return(spinwin) 
	}

# Game A: Betting on Red
alwaysbetonred <- function(){
	redspins <- 0
	redwins <- 0

	redwins <- roulettespin(-1,1)
	redspins <- redspins + 1
	return(c(redspins,redwins))
	}
	
# Game B: Betting on one number
betonenumber <- function(){
	onenumberspins <- 0
	onenumberwins <- 0
	
	# 17 is my favorite number
	onenumberwins <- roulettespin(17,1)
	onenumberspins <- onenumberspins +1
	return(c(onenumberspins,onenumberwins))
	}

# Game C: Martingale System
martingale <- function(){
	martingalespins <- 0
	martingalewins <- 0
	
	currentbet <- 1
	while(martingalewins < 10 & currentbet < 100){
		spin <- roulettespin(-1,currentbet) # the actual spin
		martingalespins <- martingalespins + 1
		martingalewins <- martingalewins + spin
		
		if(spin < 0){
			currentbet <- currentbet * 2}
			
		else if(spin > 0){
			currentbet <- 1}
		
		#print(c(martingalespins,martingalewins,currentbet))
		}
		
	return(c(martingalespins,martingalewins))
	}

# Game D: Labouchere System
labouchere <- function(){
	laboucherespins <- 0
	laboucherewins <- 0
	currentbet <- 0 # to be overwritten quite soon
	list <- c(1,2,3,4)
	
	
	
	while(length(list) != 0 & currentbet < 100){
		currentbet <- list[1] + list[length(list)] #initialize
		spin <- roulettespin(-1,currentbet)
		laboucherespins <- laboucherespins + 1
		laboucherewins <- laboucherewins + spin
		
		if(spin < 0){
			list <- c(list, currentbet)}
			
		else if (spin > 0){
			list <- list[c(-1,-length(list))]}
		
		#print(list)
		#print(c(laboucherespins,laboucherewins,currentbet))
		}
	return(c(laboucherespins,laboucherewins))
	}
	
simulateall <- function(numberoftimes){
	Atrials <- t(replicate(numberoftimes,alwaysbetonred()))
	Btrials <- t(replicate(numberoftimes,betonenumber()))
	Ctrials <- t(replicate(numberoftimes,martingale()))
	Dtrials <- t(replicate(numberoftimes,labouchere()))
	
	Awins <- length(which(Atrials[,2] > 0))/numberoftimes
	Bwins <- length(which(Btrials[,2] > 0))/numberoftimes
	Cwins <- length(which(Ctrials[,2] > 0))/numberoftimes
	Dwins <- length(which(Dspins[,2] > 0))/numberoftimes
	
	totalspins<- c(mean(Atrials[,1]),mean(Btrials[,1]),mean(Ctrials[,1]),mean(Dtrials[,1]))
	totalwins <- c(Awins,Bwins,Cwins,Dwins)
	expectedwin <- c(mean(Atrials[,2]),mean(Btrials[,2]),mean(Ctrials[,2]),mean(Dtrials[,2]))
	
	results <- cbind(expectedwin,totalwins, totalspins)
	rownames(results) <- c("Bet on red","Bet one number","Martingale","Labouchere")
	colnames(results) <- c("Expected winnings", "Win percentage","Expected bets")
	return(results)
	}
	
seed <- 110685
set.seed(seed)

# ON MY MACBOOK:
# > system.time(simulateall(100000))
#    user  system elapsed 
# 205.886   2.099 208.846 
#
# > simulateall(100000)
#                Expected winnings Win percentage Expected bets
# Bet on red              -0.03134        0.48433       1.00000
# Bet one number          -0.01936        0.02724       1.00000
# Martingale              -1.80014        0.91100      19.50408
# Labouchere              -3.80113        0.00090       8.69190

simulateall(100000)