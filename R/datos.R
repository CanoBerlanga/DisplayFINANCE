datos <-
function(y){
require(zoo)
require(timeSeries)

x<- read.csv2(file=y, sep=",", dec=".", header=TRUE,stringsAsFactors = FALSE)
x[,1]<-as.POSIXct(x[,1],format = "%d-%m-%Y" )
zoo(x[,2:dim(x)[2]],x[,1])->x
diff(log(x))*100->x
timeSeries(x)->x
return(x)
}
