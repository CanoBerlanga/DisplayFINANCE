Descriptiva <-
function (x) {
    x <- as.matrix(x)
    Output <- matrix(NA, ncol = 7, nrow = ncol(x))
    colnames(Output) <- c("Mean", "SD", "Skewness", "Kurtosis", "Q50", "VaR(5%)", "VaR(1%)")
    rownames(Output) <- colnames(x)
    for (i in 1:ncol(x)) {
        Output[i, ] <- c(
        mean(x[, i], na.rm = TRUE), 
        sd(x[, i],na.rm = TRUE), 
        skewness(x[,i], na.rm = TRUE), 
        kurtosis(x[,i], na.rm = TRUE)+3,
        quantile(x[,i], 0.5, na.rm = TRUE),
        quantile(x[,i], 0.05, na.rm = TRUE),
        quantile(x[,i], 0.01, na.rm = TRUE))
    }
    return(as.data.frame(Output))
}
