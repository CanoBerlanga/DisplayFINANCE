Explore<-function(x,a=1){
	Zak = ugarchspec(mean.model = list(armaOrder = c(0, 
        0), include.mean = TRUE, arfima = FALSE), variance.model = list(garchOrder = c(1, 
        1), model = "fGARCH", submodel = "TGARCH"), distribution.model = "ged")
    model<-ugarchfit(x,spec=Zak) 
    M<-uncmean(model)   
	SD<-uncvariance(model)^0.5
	NU<-coef(model)[6]
	
	Sim<-rsged(500000, mean = M, sd = SD, nu = NU,xi=a)
	
	
	Stats1<-Descriptiva(x)
	Stats2<-Descriptiva(Sim)
	
	M.res<-rbind(Stats1,Stats2)
	rownames(M.res)<-c("Real","Simulated")
	
	C<-cbind(M,SD,NU)
	colnames(C)<-c("Mean","SD","Shape")
	plot(density(x),xlim=c(-7,7),axes=FALSE)
	axis(1,family="Times")
	axis(2,family="Times")
	lines(density(Sim),col="steelblue",lty=2,lwd=1.5)
	abline(v=qged(0.05, mean = M, sd = SD, nu = NU),lty=2,lwd=1)
	print(M.res)
	return(C)
}