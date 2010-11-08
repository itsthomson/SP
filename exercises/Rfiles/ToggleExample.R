# Before you run this file you need to 
# source("DMBpplane.r")

# Draw phase arrows and nullclines 
phasearrows(fun=toggle,c(0,3),c(0,3),parms=c(3,2,2))
nullclines(fun=toggle,c(0,3),c(0,3),parms=c(3,2,2),add=T)

# find fixed points by starting newton's method near nullcline intersections 
xbar1=newton(Toggle,x0=c(2.5,0.4),parms=c(3,2,2));
xbar2=newton(Toggle,x0=c(1,1),parms=c(3,2,2))
xbar3=newton(Toggle,x0=c(0.4,2.5),parms=c(3,2,2)) 

# draw some illustrative trajectories 

draw.a.trajectory=function(x0,y0) {
  out=lsoda(c(x0,y0),times=seq(0,100,by=0.1),func=Toggle,parms=c(3,2,2)); 
  points(out[,2],out[,3],type="l",lwd=2,col="green"); 
}
draw.a.trajectory(0.2,0.1); draw.a.trajectory(0.1,0.2); 
draw.a.trajectory(2.8,2.6); draw.a.trajectory(2.6,2.8); 
draw.a.trajectory(2,0); draw.a.trajectory(0,2); 
draw.a.trajectory(1.5,3); draw.a.trajectory(3,1.5); 

# draw stable and unstable manifolds for the saddle point
DrawManifolds(Toggle,x0=c(1.0,1.2),parms=c(3,2,2))


