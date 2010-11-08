p=rep(0,5); 					        
cat("3 runs of geometric growth model","\n")
for (init in c(1,5,9)) {						
	p[1]=init; 				            
	for (n in 2:5) {					        	
	    p[n]=2*p[n-1]
	}
	cat("initial value = ",init,"\n"); 
	for (n in 1:5) {cat(n,p[n],"\n")};
}

					            
