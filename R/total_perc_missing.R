total_perc_missing <-
function(dtf) {
    ## First remove sex column:
    dtf <- dtf[, -1]
    ## Then compute % of missing values:
    perc <- 100 * sum(is.na(dtf)) / (nrow(dtf) * ncol(dtf))
    ## And return a rounded value:
    return(round(perc, 1))
}
