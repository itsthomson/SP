X=read.table('ChlorellaGrowth.txt'); 
X=as.matrix(X); 
Light=X[,1]; rmax=X[,2]; 

par(cex=1.5,cex.main=0.9); 
plot(Light,rmax,xlab="Light intensity (uE/m2/s)", ylab="Maximum growth rate rmax (1/d)",pch=16); 

# xlab and ylab are x and y axis labels, pch is "plotting character"
# cex is 'character expansion' - cex=1.5 increases symbol & label sizes by 50%
# cex.main sets the character expansion for the main title of the plot 

title(main="Data from Fussmann et al. (2000) system");
fit=lm(rmax~Light);
summary(fit); abline(fit); 

# Next we get the regression equation to 'display itself' on the graph
c1=round(fit$coef[1],digits=3); 	# intercept	
c2=round(fit$coef[2],digits=3); 	# slope
text(50,3,paste("rmax=",c1,"+",c2,"Light")); 

# Use ?round, ?text and ?paste to read about these commands
# for working with plots  



