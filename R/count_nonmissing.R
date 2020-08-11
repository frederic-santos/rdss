count_nonmissing <-
function(x) {
    return(sum(! is.na(x)))
}
