dss_sex_estimation <-
function(ref, target, conf = 0.95, method,
         selvar = "none", ntrees = 200, downsampling = FALSE) {
### ref : dataframe containing the reference dataset
### target: target individual
### conf : numeric value in ]0.5, 1[. Threshold pp for sex estimation
### method: string. "lda", "linda", "rf"
### selvar: string. For LDA only. One of "none", backward", "forward"
### ntrees: number of trees in RF, passed to randomForest()
### downsampling: boolean. Apply downsampling or not in RF

    if (is.null(ref)) {
        return()
    }

    ##################################
    ## 1. Prepare reference dataset ##
    ##################################
    ref_lm <- ref
    colnames(ref_lm)[1] <- "Sex"
    ref_lm$Sex <- factor(ref_lm$Sex)
    p <- ncol(ref_lm) - 1 # number of covariates

    #################################################################
    ## 2. Set up the dataframe in which the results will be stored ##
    #################################################################
    dtf_res <- matrix(NA, nrow = 7, ncol = 1)
    rownames(dtf_res) <- c("Sex estimate", "Prob(Sex==M)",
                           "Model", "Number of females in ref. sample",
                           "Number of males in ref. sample",
                           "% indeterminate (LOOCV)", "% accuracy (LOOCV)")
    colnames(dtf_res) <- rownames(target)
    dtf_res <- as.data.frame(dtf_res)

    ############################################
    ## 3. Store some constants in the results ##
    ############################################
    dtf_res[4, 1] <- nrow(ref_lm[ref_lm$Sex == "F", ])
    dtf_res[5, 1] <- nrow(ref_lm[ref_lm$Sex == "M", ])

    ###############################
    ## 4. Perform sex estimation ##
    ###############################
    if (method == "lda") {
        ## LDA:
        if (selvar != "none") {
            best_lda <- klaR::stepclass(Sex ~ ., data = ref_lm,
                                        method = "lda",
                                        direction = selvar,
                                        fold = nrow(ref_lm))
            ref_lm <- ref_lm[, c("Sex", best_lda$model$name)]
        }
        mod_cv <- MASS::lda(Sex ~ ., data = ref_lm, CV = TRUE,
                            prior = c(0.5, 0.5))
        mod_pred <- MASS::lda(Sex ~ ., data = ref_lm,
                              prior = c(0.5, 0.5))
        prediction <- predict(mod_pred, newdata = target)$posterior[, "M"]
        cv_results <- dss_loocv(mod = mod_cv, ref = ref_lm, conf = conf,
                                method = method)
        details <- coef(mod_pred)
    } else if (method == "rf") {
        ## Random forest:
        if (!downsampling) {
            mod <- randomForest(Sex ~ ., data = ref_lm,
                                ntree = ntrees,
                                classwt = c(0.5, 0.5))
        } else {
            mod <- randomForest(Sex ~ ., data = ref_lm,
                                ntree = ntrees,
                                strata = ref_lm$Sex,
                                sampsize = rep(min(table(ref_lm$Sex)), 2),
                                classwt = c(0.5, 0.5))
        }
        prediction <- predict(mod, newdata = target, type = "vote")[, "M"]
        cv_results <- dss_loocv(mod, ref = ref_lm, conf = conf,
                                method = method)
        details <- mod$importance[order(mod$importance, decreasing = TRUE), ]
        details <- data.frame(Score = details)
    }

    #######################
    ## 5. Return results ##
    #######################
    dtf_res["Model", 1] <- paste("Sex ~",
                                 paste0(colnames(ref_lm)[-1],
                                        collapse = " + "))
    dtf_res["% indeterminate (LOOCV)", 1] <- cv_results$indet_rate
    dtf_res["% accuracy (LOOCV)", 1] <- cv_results$accur_rate
    dtf_res["Prob(Sex==M)", 1] <- round(prediction, 3)
    dtf_res["Sex estimate", 1] <- dss_final_estimate(prob_m = prediction,
                                                     conf = conf)
    return(list(res_dss = dtf_res,
                table_loocv = cv_results$confusion_matrix,
                details = details))
}
