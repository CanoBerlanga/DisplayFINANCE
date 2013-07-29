Stochastic.Vol <-
function(x){
require(stochvol)
require(moments)

draws <- svsample(
	as.matrix(x-mean(x)), 
	draws = 10000, 
	burnin = 100, 
	priormu = c(-10, 1),
	priorphi = c(20, 1.2), 
	priorsigma = 0.2
	); 

St<-as.matrix((draws$summary$latent[,6]))
Res<-as.matrix(draws$summary$para)
E<-x/St
MAT<-cbind(x,St,E)
Ke<-kurtosis(E)+3
Sy<-sqrt(Res[5,1]/(1-Res[2,1]))
Ky<-Ke*exp(Sy^2)
names(MAT)<-c("y","St","Et")
Tt<-cbind(t(Res[1:3,1]),Sy,Ky)
return(list(Tt=Tt,Res=Res,St=St,Mat=MAT))
}
