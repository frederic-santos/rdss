count_na <- function(x) {
    return(sum(is.na(x)))
}

count_nonmissing <- function(x) {
    return(sum(! is.na(x)))
}

remove_na <- function(data, which = c("ind", "var"),
                      prop_min = NULL, n_min = NULL) {

##########################
### 1. Check arguments ###
##########################
    ## Either individuals or variables:
    which <- match.arg(which)
    ## Turn 'which' into (numeric) 'margin' argument for apply() below:
    if (which == "ind") {
        margin <- 1
        nb_elements <- ncol(data)
    } else {
        margin <- 2
        nb_elements <- nrow(data)
    }
    ## Both arguments cannot be NULL simultaneously:
    if (is.null(prop_min) & is.null(n_min)) {
        stop("Please provide a value for prop_min or n_min.")
    }
    ## n_min must have a correct value if it is supplied:
    if (!is.null(n_min)) {
        if (n_min < 0 | n_min > nb_elements) {
            stop("Incorrect value for n_min.")
        }
    }
    ## prop_min must be correct if it supplied:
    if (!is.null(prop_min)) {
        if (prop_min > 1 | prop_min < 0) {
            stop("Incorrect value for prop_min.")
        }
        if (!is.null(n_min)) {
            message("n_min has been used instead of prop_min.")
        } else {
            n_min <- nb_elements * prop_min
        }
    }

############################
### 2. Perform selection ###
############################
    ## NAs per element (row or column):
    cp_ok <- apply(data, MARGIN = margin, FUN = count_nonmissing)
    to_keep <- which(cp_ok >= n_min)
    ## Keep only elements that are ok:
    if (which == "ind") {
        return(data[to_keep, , drop = FALSE])
    } else {
        return(data[, to_keep, drop = FALSE])
    }
}
