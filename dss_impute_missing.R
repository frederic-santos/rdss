dss_impute_missing <- function(dtf, method = "missMDA") {
    if (method == "missMDA") {
        imp <- missMDA::imputePCA(X = dtf[, -1])$completeObs
    } else { # method = "missForest"
        imp <- missForest::missForest(xmis = dtf[, -1])$ximp
    }
    res <- data.frame(Sex = dtf[, 1], imp)
    colnames(res) <- colnames(dtf)
    return(res)
}
