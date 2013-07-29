densidades <-
function(x,y=1){
require(PerformanceAnalytics)
chart.Histogram(x[,y,drop=F], main = "Density", breaks=40, methods = c("add.density", "add.normal","add.risk"),xlim=c(-0.08,0.08))
}
