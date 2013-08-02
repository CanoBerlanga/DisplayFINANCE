carteras <-
function(x){
	require(fPortfolio)
	#Datos iniciales
	var(x)->MCV
	apply(x,2,mean)->Er
	apply(x,2,mean)->ER
	apply(x,2,sd)->ss
	as.matrix(Er)->Er
	N<-dim(MCV)[2]
	#Matriz resultados
	sim<-matrix(NA,ncol=3,nrow=ncol(x)+2)
	rownames(sim)<-c("Rendimiento","Riesgo",colnames(x))
	colnames(sim)<-c("GLOBAL","MINIMO","TANGENTE")
	#carteras
	timeSeries(x)->x
	Spec = portfolioSpec()
    setTargetReturn(Spec) = mean(colMeans(x))
    Spec2 = portfolioSpec()
     setTargetReturn(Spec2) = NULL
    Constraints = "LongOnly"
    minvariancePortfolio(x, Spec, Constraints)->c1
    efficientPortfolio(x, Spec, Constraints)->c2
    tangencyPortfolio(x, Spec, Constraints)->c3
    #maxreturnPortfolio(x, Spec, Constraints)->c4
    #Extracción Pesos
    as.matrix(getWeights(c1))->pc1
    as.matrix(getWeights(c2))->pc2
    as.matrix(getWeights(c3))->pc3
    #as.matrix(getWeights(c4))->pc4
    #Mu + Sigma
     Rpc1<-t(pc1)%*%Er
	 Spc1<-sqrt(t(pc1)%*%MCV%*%pc1)
	 Rpc2<-t(pc2)%*%Er
	 Spc2<-sqrt(t(pc2)%*%MCV%*%pc2)
	 Rpc3<-t(pc3)%*%Er
	 Spc3<-sqrt(t(pc3)%*%MCV%*%pc3)
	 #Rpc4<-t(pc4)%*%Er
	 #Spc4<-sqrt(t(pc4)%*%MCV%*%pc4)
	 #Relleno Matriz
	 cbind(pc1,pc2,pc3)->PESOS
    cbind(Rpc1,Rpc2,Rpc3)->R
    cbind(Spc1,Spc2,Spc3)->S
    sim[1,1:3]<-R
    sim[2,1:3]<-S
    sim[3:(ncol(x)+2),1:3]<-PESOS
    #######################
    Spec=portfolioSpec()
	setNFrontierPoints(Spec) = 300
    Frontier = portfolioFrontier(x,spec=Spec)
    plot(frontierPoints(Frontier,frontier="upper"),
     xlim=c(Spc1-Spc1/8,max(ss)),
     ylim=c(min(ER),max(ER)),
     col="dodgerblue3",
        lwd=2,
        ylab=expression(mu[c]),
        xlab=expression(sigma[c]),
        #main="Frontera Eficiente",
        axes=FALSE,
    type="l"
    )
    axis(1,family="Times",pos=Rpc1,labels=FALSE)
    axis(2,family="Times")
    #######################
            points(ss,ER,pch=22)
           text(ss,ER+0.00015, colnames(x),cex=0.65,pos=2,col="black",family="Times")
           points(Spc3,Rpc3,pch=22)
           sharpeRatioLines(Frontier, col = "grey", lwd = 1.5,lty=1,family="Times")
         slope = (Rpc3 - 0)/(Spc3)    
         abline(0, slope,lty=2,lwd=1,col="dodgerblue4");points(Spc3,Rpc3,pch=18,col="black")

return(sim)
}
