check_data_dss <- function(dat, sex, females, males, tbd) {

    if (ncol(dat) <= 3) {
        ## 1. Check that there are more than three columns:
        showModal(modalDialog(
            title = "Invalid data file",
            "There are not enough columns. Check your field separator.",
            easyClose = TRUE
        ))
        return()
    } else if (max(table(dat[, 1])) > 1) {
        ## 2. Check that the first column contains valid row names:
        showModal(modalDialog(
            title = "Invalid data file",
            "Invalid row names. Check for duplicates.",
            easyClose = TRUE
        ))
        return()
    } else {
        ## The df is valid, thus return it:
        rownames(dat) <- dat[, 1]
        dat[, 1] <- NULL
        return(dat)
    }
    ## There is a column for Sex:
    ## There are three levels in this factor:
    ## There is a sufficient number of individuals:
}
