Descriptiva <-
function (x) {
    x <- as.matrix(x)
    Output <- matrix(NA, ncol = 7, nrow = ncol(x))
    colnames(Output) <- c("Mean", "SD", "Skewness", "Kurtosis", "Q25", "Q50", "Q75")
    rownames(Output) <- colnames(x)
    for (i in 1:ncol(x)) {
        Output[i, ] <- c(
        mean(x[, i], na.rm = TRUE), 
        sd(x[, i],na.rm = TRUE), 
        skewness(x[,i], na.rm = TRUE), 
        kurtosis(x[,i], na.rm = TRUE),
        quantile(x[,i], 0.25, na.rm = TRUE),
        quantile(x[,i], 0.5, na.rm = TRUE), 
        quantile(x[,i], 0.75, na.rm = TRUE))
    }
    return(as.data.frame(Output))
}
