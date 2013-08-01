Explore<-function(x,a=1){
	Zak = ugarchspec(mean.model = list(armaOrder = c(0, 
        0), include.mean = TRUE, arfima = FALSE), variance.model = list(garchOrder = c(1, 
        1), model = "fGARCH", submodel = "TGARCH"), distribution.model = "ged")
    model<-ugarchfit(x,spec=Zak) 
    M<-uncmean(model)   
	SD<-uncvariance(model)^0.5
	NU<-coef(model)[6]
	Sim<-rsged(900000, mean = M, sd = SD, nu = NU,xi=a)
	VAR<-qsged(0.05, mean = M, sd = SD, nu = NU,xi=a)
	VARh<-quantile(x,p=0.05)
	VARM<-cbind(VAR,VARh)
	colnames(VARM)<-c("GED","Historical")
	Stats1<-Descriptiva(x)
	Stats2<-Descriptiva(Sim)
	M.res<-rbind(Stats1,Stats2)
	rownames(M.res)<-c("Observed","Simulated")
	C<-cbind(M,SD,NU)
	colnames(C)<-c("Mean","SD","Shape")
	rownames(C)<-c("GED")
	
    plot(density(x),
    xlim=c(-7,7),
    axes=FALSE,
    lty=1,
    lwd=1.5,
    col="#AEA79F",
    main="Returns Distribution",
    xlab=expression(y[t]),
    family="Times")
    
	axis(1,family="Times")
    axis(2,family="Times")
    
	lines(density(Sim),
    col="#0191C8",
    lty=1,lwd=1.5)
    
	abline(v=VAR,lty=2,lwd=0.5)
    text(VAR, 0.005, paste("Value at Risk = ",round(VAR,digits=4),sep=""), offset = 0.5, pos = 4, cex = 0.8, srt = 0,family="Times")
	return(
	list(Parameters=C,VaR=VARM,Statistics=M.res)
	)
}