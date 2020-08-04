dss_loocv <- function(mod, ref, conf = 0.95, br = TRUE) {
### mod : glm model (glm or logistf object)
### ref : dataframe. Reference dataset
### conf : numeric value. PP threshold for sex estimation
### br : boolean. Apply Firth's correction or not.
### output -> vector of length 2: c(%indet, %accuracy).

    ##########################################
    ## 1. Set up the table of LOOCV results ##
    ##########################################
    tab_cv <- matrix(NA, nrow = nrow(ref), ncol = 3)
    colnames(tab_cv) <- c("ProbM", "Sex_estimate", "True_sex")
    tab_cv <- as.data.frame(tab_cv)
    tab_cv[, "True_sex"] <- ref$Sex

    #####################################
    ## 2. Perform LOOCV sex estimation ##
    #####################################
    for (i in seq_len(nrow(ref))) {
        if (br == TRUE) {
            mod_cv <- logistf::logistf(Sex ~ ., data = ref[-i, ])
        } else {
            mod_cv <- glm(Sex ~ ., data = ref[-i, ], family = binomial)
        }
        prob_m <- predict(mod_cv, newdata = ref[i, ], type = "response")
        tab_cv[i, "ProbM"] <- round(prob_m, 3)
        tab_cv[i, "Sex_estimate"] <- dss_final_estimate(prob_m, conf = conf)
    }

    #################################
    ## 3. Compute and return rates ##
    #################################
    cm <- table(ref$Sex, tab_cv$Sex_estimate) # confusion matrix
    ## Rate of reference indivs remaining indeterminate in LOOCV:
    indet_rate <- nrow(tab_cv$Sex_estimate == "I") / nrow(tab_cv)
    ## Rate of reference indivs correctly classified:
    deter <- subset(tab_cv, Sex_estimate != "I")
    accur_rate <- sum(deter[, 2] == deter[, 3]) / nrow(deter)

    return(list(indet_rate = perc_indet,
                accur_rate = success_rate,
                confusion_matrix = cm))
}
