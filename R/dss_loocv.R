dss_loocv <- function(mod, ref, conf = 0.95, method = "lda",
                      alpha = NULL, lambda = NULL,
                      linda_alpha = NULL) {
### mod : model (glmnet, lda, or randomForest object)
### ref : dataframe. Reference dataset
### conf : numeric value. PP threshold for sex estimation
### method: string. "glmnet", "lda", "linda", "rf"
### alpha: for method = "glmnet" only; alpha value used.
### lambda: for method = "glmnet" only; lambda value used.
### linda_alpha: for method = "linda" only; passed to rrcov::Linda()
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
    ## Perform or extract LOOCV according to "method":
    if (method == "glmnet") {
        for (i in seq_len(nrow(ref))) {
            temp <- ref[-i, ]
            targ <- ref[i, ]
            modcv <- glmnet::glmnet(x = as.matrix(temp[, -1]),
                                    y = temp$Sex,
                                    family = "binomial",
                                    alpha = alpha,
                                    lambda = lambda)
            tab_cv[i, "ProbM"] <- predict(modcv,
                                          newx = as.matrix(targ[, -1]),
                                          type = "response")
        }
    } else if (method == "lda") {
        tab_cv[, "ProbM"] <- mod$posterior[, "M"]
    } else if (method == "rf") {
        tab_cv[, "ProbM"] <- mod$votes[, "M"]
    } else if (method == "linda") {
        for (i in seq_len(nrow(ref))) {
            temp <- ref[-i, ]
            targ <- ref[i, ]
            modcv <- rrcov::Linda(x = as.matrix(temp[, -1]),
                                  grouping = temp$Sex,
                                  prior = c(0.5, 0.5),
                                  alpha = linda_alpha)
            tab_cv[i, "ProbM"] <- predict(modcv, as.matrix(targ[, -1]))@posterior[1, "M"]
        }
    }

    ## In all cases, compute final sex estimate according to "conf" threshold:
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
    deter <- tab_cv[tab_cv$Sex_estimate != "I", ]
    accur_rate <- sum(deter[, 2] == deter[, 3]) / nrow(deter)

    return(list(indet_rate = round(100 * indet_rate, 1),
                accur_rate = round(100 * accur_rate, 1),
                confusion_matrix = cm))
}
