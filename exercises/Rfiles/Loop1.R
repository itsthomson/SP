# initial population size
initsize=4; 

# create vector to hold results and store initial size 
popsize=rep(0,10); popsize[1]=initsize;

# calculate population size at times 2 through 10, write to Command Window
for (n in 2:10 ) { 
	popsize[n]=2*popsize[n-1];
	x=log(popsize[n]);
	cat(n,x,"\n");
}
