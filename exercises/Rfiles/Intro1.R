Light=c(20,20,20,20,21,24,44,60,90,94,101)
rmax=c(1.73,1.65,2.02,1.89,2.61,1.36,2.37,2.08,2.69,2.32,3.67)
plot(Light, rmax,cex=1.5,pch=16); 
fit=lm(rmax~Light);
summary(fit); abline(fit); 

