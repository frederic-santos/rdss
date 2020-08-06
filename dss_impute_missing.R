dss_impute_missing <- function(dtf, method = "missMDA") {
    if (total_perc_missing(dtf) == 0) {
        return(dtf)
    } else if (method == "missMDA") {
        nb <- missMDA::estim_ncpPCA(dtf[, -1])
        imp <- missMDA::imputePCA(X = dtf[, -1], ncp = nb$ncp)$completeObs
    } else { # method = "missForest"
        imp <- missForest::missForest(xmis = dtf[, -1])$ximp
    }
    res <- data.frame(Sex = dtf[, 1], imp)
    colnames(res) <- colnames(dtf)
    return(res)
}
