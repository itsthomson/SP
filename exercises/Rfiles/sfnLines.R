library(odesolve);

sfn2=function(t,y,parms) {
	e=parms[1]; b=parms[2]; dx2=parms[3];
	u=y[1:ny2]; v=y[(ny2+1):ny]; 
	umat=matrix(u,nxy,nxy); 
	uer = cbind(umat[,1],umat,umat[,nxy]); # u with extra rows
	uec = rbind(umat[1,],umat,umat[nxy,]); # u with extra columns 
	ul = uec[3:(nxy+2),]+uec[1:nxy,]+uer[,1:nxy]+uer[,3:(nxy+2)]-4*umat;
	uf = (u-0.3333*u*u*u-v)/e + dx2*matrix(ul,ny2,1);
	vf = e*(u + b-0.5*v);
   	return(list(dy=c(uf,vf)))
}	


# Parameters and initial conditions
nxy = 50; ny2=nxy*nxy; ny=2*ny2;  
dx2 = 1/9; h  = 0.04; e = 0.1; b = 0.67;
u=5*outer(rep(1,nxy),((1:nxy)-0.4*nxy)/nxy); 
v = t(u); 

y0=c(as.vector(u),as.vector(v));
for(i in 1:1000) { 
	out=rk4(y0,times=seq(0,h,length=2),func=sfn2,parms=c(e,b,dx2)); 
	y0=out[2,-1]; 
	u=matrix(y0[1:ny2],nxy,nxy); 
	if(i%%4==1) image(1:dim(u)[2],1:dim(u)[1],t(u),col=rainbow(n=100,start=0,end=0.7))
}


