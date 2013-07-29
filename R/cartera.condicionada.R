cartera.condicionada <-
function(x,y){
	x <- rmvnorm(n=5000, mean=colMeans(x), sigma=y)
	return(carteras(x))
	}
