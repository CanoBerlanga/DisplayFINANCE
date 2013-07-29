tails<-function(x){
pos<-Descriptiva(x[x>0])
neg<-Descriptiva(x[x<0])
res<-rbind(pos,neg)
rownames(res)<-c("R(+)","R(-)")

x[x>0]<-x[x>0]+10
x[x<0]<-x[x<0]-10

plot(density(x),lwd=2,col="dodgerblue3",axes=FALSE)
axis(1)
axis(2)
abline(v=0,lty=2)

return(res)
}