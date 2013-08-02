Probabilidad<-function(x,p){
	x<-as.matrix(x)
	m<-p[,1]
	sd<-p[,2]
	sh<-p[,3]
	s = seq(min(x), max(x), length = length(x))
	x<-x[order(x[,1]),]
	plot(x,
	(1:length(x)/length(x)), 
	main = "Probability", 
	col = "red",type="l",lwd=2,lty=1,
     ylab = "Probability")
   lines(s, pged(s,m,sd,sh), lwd = 2)
}