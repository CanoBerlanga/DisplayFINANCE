mgarch <-
function(x){
	require(gogarch)
	h<-dim(x)[1]
	gognls <- gogarch(x, formula = ~garch(1,1), scale = TRUE,estby = "nls",garchlist=
	list(
	include.mean=FALSE,
	leverage=TRUE,
	con.dist="norm"
	))
	gognls@H[[h]]->MCV
	return(MCV)
}
