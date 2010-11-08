p=rep(0,5); 					        
for (init in c(1,5,9)) {						
	p[1]=init; 				            
	for (n in 2:5) {					        	
	    p[n]=2*p[n-1]
	    cat(init,n,p[n],"\n"); 	  
        }
}						            
