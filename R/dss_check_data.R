dss_display_error <- function(message, title, mode) {
### Internal function.
### message: contents of error message to be displayed
### title: popup title if mode == "shiny"
### mode: either "console" (text display) or "shiny" (popup display through UI)
    if (mode == "console") {
        stop(paste(title, ". ", message,
                   sep = ""),
             call. = FALSE)
    } else { # popup in UI
        showModal(modalDialog(
            title = title,
            message,
            easyClose = TRUE))
    }
}

dss_check_data <-
    function(file, sex, females, males, tbd,
             rm_empty_rows = FALSE,
             mode = "console") {
### file: dataframe uploaded by the user
### sex: string, colname for Sex factor in 'file'
### females, males, tbd: strings, abreviations in Sex factor.
### rm_empty_rows: boolean. Indicates what to do with empty individuals.
### mode: either "console" or "UI": final user must use the default.
### Return an error message if 'file' is not suitable.
### Otherwise, return `file' with Sex as its 1st column.

    if (ncol(file) < 3) {
        ## 1. Check that there are at least three columns:
        dss_display_error(
            title = "Invalid data file",
            message = "There are not enough columns. Check your field separator.",
            mode = mode
        )
        return()
    } else if (max(table(file[, 1])) > 1) {
        ## 2. Check that the first column contains valid row names:
        dss_display_error(
            title = "Invalid data file",
            message = "Invalid row names. Check for duplicates.",
            mode = mode
        )
        return()
    } else if (! sex %in% colnames(file)) {
        ## 3. Check that there is a column for Sex:
        dss_display_error(
            title = "Invalid sex column",
            message = paste("There is no column \u201c",
                            sex,
                            "\u201d in your data file.",
                            sep = ""),
            mode = mode
        )
        return()
    } else if (nlevels(file[, sex]) != 3) {
        ## 4. Check that there are three levels in this Sex factor:
        dss_display_error(
            title = "Invalid coding for the Sex factor",
            message = paste("There should be exactly three levels",
                            "in your column",
                            sex,
                            "(one for females, one for males, and one for",
                            "target individuals).",
                            "You have currently",
                            nlevels(file[, sex]),
                            "levels for this column.",
                            "Please check your data file."),
            mode = mode
        )
        return()
    } else if (any(! levels(file[, sex]) %in% c(females, males, tbd))) {
        ## 5. Check that factor levels are correct:
        dss_display_error(
            title = "Invalid coding for the Sex factor",
            message = paste("The three levels for Sex factor in your file (",
                            paste0(levels(file[, sex]), collapse = ", "),
                            ") do not match the three levels you indicate in the ",
                            "user interface.",
                            sep = ""),
            mode = mode
        )
        return()
    } else if (nrow(file) <= 6) {
        ## 6. Check that there is a sufficient number of individuals:
        dss_display_error(
            title = "Your learning sample is too small!",
            message = paste("This would not be reasonable to perform sex",
                            "estimation with such a small learning sample.",
                            "More data are necessary."),
            mode = mode
        )
        return()
    } else {
        ## The df is valid, thus return it and put Sex in 1st column:
        rownames(file) <- file[, 1]
        file[, 1] <- NULL
        dat_wt_sex <- file[, colnames(file) != sex, drop = FALSE]
        file <- data.frame(Sex = file[, sex],
                           dat_wt_sex)
        colnames(file)[1] <- sex
        ## Furthermore, standardise factor levels:
        levels(file[, sex])[levels(file[, sex]) == females] <- "F"
        levels(file[, sex])[levels(file[, sex]) == males] <- "M"
        ## Finally, should individuals with all values missing be removed?
        if (rm_empty_rows == TRUE) {
            nb_na <- apply(file, MARGIN = 1, FUN = count_na)
            discard <- (nb_na >= (ncol(file) - 1))
            file <- file[!discard, ]
        }
        ## Return valid data file:
        return(file)
    }
}
