dss_final_estimate <- function(prob_m, conf = 0.95) {
    if (prob_m >= conf) {
        return("M")
    } else if (prob_m <= 1 - conf) {
        return("F")
    } else {
        return("I")
    }
}
