Evolution<-function(x){
	
	plot(as.ts(cumsum(x)),
		lwd=2,
		col="#0191C8",
		axes=FALSE,
		family="Times"
	)
	
	axis(1,family="Times")
	axis(2,family="Times")
	
}