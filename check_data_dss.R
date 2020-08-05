check_data_dss <- function(file, sex, females, males, tbd) {
### file: dataframe uploaded by the user
### sex: string, colname for Sex factor in `file'
### females, males, tbd: strings, abreviations in Sex factor.
### Return an error message if `file' is not suitable.
### Otherwise, return `file' with Sex as its 1st column.

    if (ncol(file) <= 3) {
        ## 1. Check that there are more than three columns:
        showModal(modalDialog(
            title = "Invalid data file",
            "There are not enough columns. Check your field separator.",
            easyClose = TRUE
        ))
        return()
    } else if (max(table(file[, 1])) > 1) {
        ## 2. Check that the first column contains valid row names:
        showModal(modalDialog(
            title = "Invalid data file",
            "Invalid row names. Check for duplicates.",
            easyClose = TRUE
        ))
        return()
    } else if (! sex %in% colnames(file)) {
        ## 3. Check that there is a column for Sex:
        showModal(modalDialog(
            title = "Invalid sex column",
            paste("There is no column “",
                  sex,
                  "” in your data file.", sep = ""),
            easyClose = TRUE
        ))
        return()
    } else if (nlevels(file[, sex]) != 3) {
        ## 4. Check that there are three levels in this Sex factor:
         showModal(modalDialog(
            title = "Invalid coding for the Sex factor",
            paste("There should be exactly three levels",
                  "in your column",
                  sex,
                  "(one for females, one for males, and one for",
                  "target individuals).",
                  "You have currently",
                  nlevels(file[, sex]),
                  "levels for this column.",
                  "Please check your data file."),
            easyClose = TRUE
        ))
        return()
    } else if (any(! levels(file[, sex]) %in% c(females, males, tbd))) {
        ## 5. Check that factor levels are correct:
         showModal(modalDialog(
            title = "Invalid coding for the Sex factor",
            paste("The three levels for Sex factor in your file (",
                  paste0(levels(file[, sex]), collapse = ", "),
                  ") do not match the three levels you indicate in the ",
                  "user interface.", sep = ""),
            easyClose = TRUE
        ))
        return()
    } else if (nrow(file) <= 6) {
        ## 6. Check that there is a sufficient number of individuals:
        showModal(modalDialog(
            title = "Your learning sample is too small!",
            paste("This would not be reasonable to perform sex estimation",
                  "with such a small learning sample.",
                  "More data are necessary."),
            easyClose = TRUE
        ))
        return()
    } else {
        ## The df is valid, thus return it and put Sex in 1st column:
        rownames(file) <- file[, 1]
        file[, 1] <- NULL
        dat_wt_sex <- file[, colnames(file) != sex]
        file <- data.frame(Sex = file[, sex],
                           dat_wt_sex)
        colnames(file)[1] <- sex
        ## Furthermore, standardise factor levels:
        levels(file[, sex])[levels(file[, sex]) == females] <- "F"
        levels(file[, sex])[levels(file[, sex]) == males] <- "M"
        return(file)
    }
}
