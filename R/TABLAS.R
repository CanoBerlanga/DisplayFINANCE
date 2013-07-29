TABLAS <-
function(x){
print(xtable(data.frame(row = rownames(x),data.frame(x)),digits=3),include.rownames = FALSE)
}
