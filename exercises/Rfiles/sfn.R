require(matlab); 
sfn=function(u,v) {
	ny = dim(u)[1]; nx = dim(u)[2];
	uer = cbind(u[,1],u,u[,nx]); # u with extra rows
	uec = rbind(u[1,],u,u[ny,]); # u with extra columns 
	ul = uec[3:(ny+2),]+uec[1:ny,]+uer[,1:nx]+uer[,3:(nx+2)]-4*u;
	uf = (u-0.3333*u*u*u-v)/e + dx2*ul;
	vf = e*(u + b-0.5*v);
	return(list(uf=uf,vf=vf)); 
} 

sfn.run=function(nsteps,init) {
   u=init$u; v=init$v; 
   for (i in 1:nsteps){
	out=sfn(u,v);
	u = u+h*out$uf;	v = v+h*out$vf;
	if (i%%5 == 1) image(1:dim(u)[2],1:dim(u)[1],t(u),col=rainbow(n=100,start=0,end=0.7))
   }
   return(list(u=u,v=v)); 
}

# Parameters and initial conditions: set 1
nxy = 100; dx2 = 1/4; h  = 0.04; e = 0.1; b = 0.67;
u=5*outer(rep(1,nxy),((1:nxy)-0.4*nxy)/nxy); 
v = t(u); 
init1=list(u=u,v=v); 
sfn.run(250,init2);


# Parameters and initial conditions: set 2
nxy = 60; dx2 = 1/9; h  = 0.04; e = 0.1; b = 0.67;
v=5*outer( (1:nxy)-0.4*nxy, rep(1,2*nxy))/nxy; 
u = 5*ones(nxy,1)%*%((1:nxy)-0.4*nxy)/nxy;
u = cbind(u,fliplr(u)) 
init2=list(u=u,v=v); 





