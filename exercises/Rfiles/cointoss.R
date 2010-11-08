nt = 1000000;
p = 0.6;      # probability of heads
r = runif(nt+1); 
rd = (r < p); # tails = 0, heads = 1
rdiff = rd[1:nt]+2*rd[2:(nt+1)];           # code transitions: tt = 0, ht = 1, th = 2, hh = 3
ht = which(rdiff == 1);                    # heads to tails indices
th = which(rdiff == 2);                    # tails to heads indices
rn  = min(length(ht),length(th));          # numbers of transitions

###Compute residence times from ht and th transition times 
if (rd[1]==0) {                             # if tails first
   rh = ht[1:rn] - th[1:rn];                  # heads residence times
   rt = c(th[1],th[2:rn] - ht[1:rn-1]);       # tails residence times
}else{                                      # if heads first
   rh = c(ht[1],ht[2:rn] - th[1:rn-1]);       # heads residence times
   rt = th[1:rn] - ht[1:rn];                  # tails residence times
}

### Plot results. 
par(mfrow=c(3,1),cex.axis=1.35,cex.lab=1.35); 
hhist = hist(rh,(0:max(rh))+0.5,xlim=c(1,max(rh)));     # heads histogram
thist = hist(rt,(0:max(rt)+0.5),xlim=c(1,max(rt)));     # tails histogram

# Both histograms on semilog scale 
xh=hhist$mids; yh=log(hhist$counts)-log(nt);
xt=thist$mids; yt=log(thist$counts)-log(nt);
xlim=range(c(xh,xt)); ylim=range(c(yh,yt),finite=TRUE);  #nice axis limits  
plot(xh,yh,type="o",col="blue",xlim=xlim,ylim=ylim,xlab="Residence time",ylab="log Frequency");
points(xt,yt,type="o",col="red"); 
title(main="Residence time frequencies on semilog scale")
