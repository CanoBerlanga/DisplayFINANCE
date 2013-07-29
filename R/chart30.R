chart30 <-
function(x,y=30){
require(PerformanceAnalytics)
N<-length(x)
charts.PerformanceSummary(x[(N-y):N,],methods="ModifiedVaR",colorset="dodgerblue3")
}
