dss_sex_estimation <- function(ref, target, conf = 0.95, method) {
### ref : dataframe containing the reference dataset
### target: target individual
### conf : numeric value in ]0.5, 1[. Threshold pp for sex estimation
### method: string. "LDA", "RF"

    if (is.null(ref)) {
        return()
    }

    ##################################
    ## 1. Prepare reference dataset ##
    ##################################
    ref_lm <- ref
    colnames(ref_lm)[1] <- "Sex"
    ref_lm$Sex <- factor(ref_lm$Sex)

    #################################################################
    ## 2. Set up the dataframe in which the results will be stored ##
    #################################################################
    dtf_res <- matrix(NA, nrow = 1, ncol = 7)
    colnames(dtf_res) <- c("Model", "Number of females in ref sample",
                           "Number of males in ref sample",
                           "%indeterminate (LOOCV)", "%accuracy (LOOCV)",
                           "Sex estimate", "Prob(Sex==M)")
    rownames(dtf_res) <- rownames(target)
    dtf_res <- as.data.frame(dtf_res)

    ############################################
    ## 3. Store some constants in the results ##
    ############################################
    dtf_res[, 2] <- nrow(ref_lm[ref_lm$Sex == "F", ])
    dtf_res[, 3] <- nrow(ref_lm[ref_lm$Sex == "M", ])

    ###############################
    ## 4. Perform sex estimation ##
    ###############################
    if (method == "LDA") {
        mod_cv <- MASS::lda(Sex ~ ., data = ref_lm, CV = TRUE,
                            prior = c(0.5, 0.5))
        mod_pred <- MASS::lda(Sex ~ ., data = ref_lm,
                              prior = c(0.5, 0.5))
        prediction <- predict(mod_pred, newdata = target)$posterior[, "M"]
        cv_results <- dss_loocv(mod = mod_cv, ref = ref_lm, conf = conf,
                                method = method)
    } else if (method == "RF") {
        mod <- randomForest(Sex ~ ., data = ref_lm,
                            classwt = c(0.5, 0.5))
        prediction <- predict(mod, newdata = target, type = "vote")[, "M"]
        cv_results <- dss_loocv(mod, ref = ref_lm, conf = conf,
                                method = method)
    }

    #######################
    ## 5. Return results ##
    #######################
    dtf_res[1, "Model"] <- paste("Sex ~",
                                 paste0(colnames(ref_lm[, -1]),
                                        collapse = "+"))
    dtf_res[1, "%indeterminate (LOOCV)"] <- cv_results$indet_rate
    dtf_res[1, "%accuracy (LOOCV)"] <- cv_results$accur_rate
    dtf_res[1, "Prob(Sex==M)"] <- round(prediction, 3)
    dtf_res[1, "Sex estimate"] <- dss_final_estimate(prob_m = prediction,
                                                     conf = conf)
    return(list(res_dss = t(dtf_res),
                table_loocv = cv_results$confusion_matrix))
}
