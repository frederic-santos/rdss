dss_min_fm <- function(dtf, female, male) {
    if (! is.null(dtf)) {
        nb_f <- sum(dtf[, 1] == female)
        nb_m <- sum(dtf[, 1] == male)
        return(min(c(nb_f, nb_m)))
    } else {
        return(NULL)
    }
}
