dss_loocv <- function(mod, ref, conf = 0.95, method) {
### mod : glm model (glm or logistf object)
### ref : dataframe. Reference dataset
### conf : numeric value. PP threshold for sex estimation
### method: string. "LDA", "robust_LDA", "RF"
### output -> vector of length 2: c(%indet, %accuracy).

    ##########################################
    ## 1. Set up the table of LOOCV results ##
    ##########################################
    tab_cv <- matrix(NA, nrow = nrow(ref), ncol = 3)
    colnames(tab_cv) <- c("ProbM", "Sex_estimate", "True_sex")
    tab_cv <- as.data.frame(tab_cv)
    tab_cv[, "True_sex"] <- ref$Sex

    #####################################
    ## 2. Extract LOOCV sex estimation ##
    #####################################
    if (method %in% c("LDA", "robust_LDA")) {
        tab_cv[, "ProbM"] <- mod$posterior[, "M"]
    } else if (method == "RF") {
        tab_cv[, "ProbM"] <- mod$votes[, "M"]
    }
    for (i in seq_len(nrow(tab_cv))) {
        tab_cv[i, "Sex_estimate"] <- dss_final_estimate(tab_cv[i, 1],
                                                        conf = conf)
    }

    #################################
    ## 3. Compute and return rates ##
    #################################
    ## Confusion matrix:
    cm <- as.data.frame.matrix(table(ref$Sex, tab_cv$Sex_estimate))
    ## Rate of reference indivs remaining indeterminate in LOOCV:
    indet_rate <- sum(tab_cv$Sex_estimate == "I") / nrow(tab_cv)
    ## Rate of reference indivs correctly classified:
    deter <- subset(tab_cv, Sex_estimate != "I")
    accur_rate <- sum(deter[, 2] == deter[, 3]) / nrow(deter)

    return(list(indet_rate = round(100 * indet_rate, 1),
                accur_rate = round(100 * accur_rate, 1),
                confusion_matrix = cm))
}
