volatilidad <-
function (x, z = "solnp") {
    require(rugarch)
    
    y = ugarchspec(mean.model = list(armaOrder = c(0, 
        0), include.mean = TRUE, external.regressors = NULL, 
        archpow = 1, arfima = FALSE), variance.model = list(garchOrder = c(1, 
        1), model = "fGARCH", submodel = "TGARCH"), distribution.model = "std")

    
    
    
    m <- dim(x)[2]
    outputQ <- NULL
    outputU <- NULL
    bayesQ <- NULL
    for (i in 1:m) {
        X <- x[, i]
        GARCH <- ugarchfit(X, spec = y, solver = z)
        GARCH.q <- GARCH@fit$coef
        pval<- t(GARCH@fit$robust.matcoef[,4])
        bayesQ <- rbind(bayesQ, t(as.matrix(c(GARCH@fit$coef, 
            uncvariance(GARCH)^0.5, halflife(GARCH)))))
       
        outputU <- rbind(outputU, pval)
    }
    bayesQ <- as.matrix(bayesQ)
    rownames(bayesQ)<-colnames(x)
    colnames(bayesQ) <- c(rownames(GARCH@fit$robust.matcoef),"Sy", "h2l")

    print(bayesQ)
    return <- list( PVAL = outputU, VALORES = bayesQ)

}
