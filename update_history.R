update_history <- function(tab, criterion, value) {
    if (is.null(tab)) {
        tab <- data.frame(Criterion = criterion,
                          Value = value)
    } else {
        tab <- rbind(tab, c(criterion, value))
    }
    return(tab)
}
