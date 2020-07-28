check_data_dss <- function(dat, sex, females, males, tbd) {
    ## There are more than three columns:
    if (ncol(dat) <= 3) {
        showModal(modalDialog(
            title = "Invalid data file",
            "There are not enough columns. Check your field separator.",
            easyClose = TRUE
        ))
        return()
    }

    ## The first column contains valid row names:
    if (max(table(dat[, 1])) == 1) {
        rownames(dat) <- dat[, 1]
        dat[, 1] <- NULL
    } else {
        showModal(modalDialog(
            title = "Invalid data file",
            "Invalid row names. Check for duplicates.",
            easyClose = TRUE
        ))
        return()
    }

    ## There is a column for Sex:
    ## There are three levels in this factor:
    ## There is a sufficient number of individuals:
    ## Return final dataframe:
    return(dat)
}
