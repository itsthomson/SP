nt = 100000;
A = matrix(c(0.98, 0.10, 0, 0.02, 0.7, 0.05, 0, 0.2, 0.95),3,3,byrow=T);
B = apply(A,2,cumsum); #cumulative sums of each column 
A; B; 
states=numeric(nt+1); rd=runif(nt);  
states[1] = 3; # Start in open state
for(i in 1:nt) {
   b=B[,states[i]]; #cumulative probabilities for current state  
   states[i+1]=sum(rd[i]>b)+1 # do the ``coin toss'' based on current state 
}
plot(states[1:1000],type="s");