dss_impute_missing <- function(dtf, method = "missMDA") {
### dtf: dataframe
### method: either "missMDA" or "missForest"

    if (total_perc_missing(dtf) == 0) {
        ## No missing values: return dtf itself
        return(dtf)
    } else if (ncol(dtf) == 2) {
        ## One single metric variable: casewise deletion
        ## (ncol == 2 because Sex also counts as a column)
        return(na.omit(dtf))
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
