# Dynamic Models in Biology, Stephen Ellner and John Guckenheimer
# TwoState3.R simulates a group of particles all moving 
# back and forth between two compartments, using the Gillespie algorithm
# to simulate exactly the continuous-time process. 
# For a large number of particles, the process should approximate
# a two-compartment differential equation
#
#   dQ1/dt= -R*Q1 + L*Q2      dQ2/dt= -L*Q2 + R*Q11
#
# Simulations of the Markov Chain are plotted along with solutions
# of the differential equation defined in the file diffus2.m

# parameters
R=0.3; L=0.2; tmax=25; 

# initial condition
nL0=10000; nR0=0; ntot=nL0+nR0; 

# initialize variables for current population size
nL=nL0; nR=nR0; 

# initialize vectors storing the results over time
ntL=nL; ntR=nR;  t=0; tvals=0;

while(t<tmax) {
   rate=nL*R+nR*L; 
   dt=rexp(1,rate); 
   if(runif(1) < nL*R/rate) {
	nL=nL-1; nR=nR+1
   }else{
	nL=nL+1; nR=nR-1
   }	  
   # store the results for plotting
   	t=t+dt; tvals=c(tvals,t);
	ntL=c(ntL,nL); ntR=c(ntR,nR);
} 

# solve the ODEs for the continuous time chain (dt -> 0)
diffus2=function(t,y,parms) {
  dy= c( -R*y[1]+L*y[2], R*y[1]-L*y[2] );
  return(list(dy))
}	 
require(odesolve); 
out=lsoda(y=c(nL0,nR0),times=seq(0,tmax,by=.1),func=diffus2,parms=0);

# plot stochastic simulation and deterministic solutions
par(cex.axis=1.25,cex.lab=1.25); 
matplot(tvals,cbind(ntL,ntR),type="l",lty=1,lwd=2,ylim=c(0,ntot), xlab="time",ylab="Particles")

matpoints(out[,1],out[,2:3],type="l",lty=1,lwd=2); 	 
title(main="Two-compartment diffusion")
legend(0.5*tmax,ntot,legend=c("Left","Right"),col=c("Black","Red"),lty=1,lwd=2)

