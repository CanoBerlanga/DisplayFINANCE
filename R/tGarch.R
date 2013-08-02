TGARCH<-function (x) 
{
    Zak = ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = TRUE, 
        arfima = FALSE), variance.model = list(garchOrder = c(1, 
        1), model = "fGARCH", submodel = "TGARCH"), distribution.model = "ged")
    model <- ugarchfit(x, spec = Zak)
    return(model)}