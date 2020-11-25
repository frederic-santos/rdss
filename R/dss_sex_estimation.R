dss_sex_estimation <-
function(ref, target, conf = 0.95, method = "lda",
         lda_selvar = "none",
         rf_ntrees = 200, rf_downsampling = FALSE,
         glmnet_type = 0, glmnet_measure = "deviance",
         linda_alpha = 0.9) {
### ref : dataframe containing the reference dataset
### target: target individual
### conf : numeric value in ]0.5, 1[. Threshold pp for sex estimation
### method: string. One of "glmnet", "lda", "linda", "rf"
### lda_selvar: string. For LDA only. One of "none", backward", "forward"
### rf_ntrees: number of trees in RF, passed to randomForest()
### rf_downsampling: boolean. Apply downsampling or not in RF
### glmnet_type: 0 or 1 for ridge or lasso; passed to glmnet()
### glmnet_measure: one of "deviance" or "class"; passed to cv.glmnet()
### linda_alpha: numeric value. alpha parameter passed to rrcov::Linda()

    ## Pre-processing:
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
    target <- target[, colnames(ref)[-1]]

    #################################################################
    ## 2. Set up the dataframe in which the results will be stored ##
    #################################################################
    dtf_res <- matrix(NA, nrow = 9, ncol = 1)
    rownames(dtf_res) <- c("Sex estimate", "Prob(Sex==M)",
                           "Model", "Number of females in ref. sample",
                           "Number of males in ref. sample",
                           "% indeterminate (LOOCV)", "% accuracy (LOOCV)",
                           "% accuracy for females (LOOCV)",
                           "% accuracy for males (LOOCV)")
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
        ## 4.1. LDA:
        ## Perform variable selection if required:
        if (lda_selvar != "none") {
            best_lda <- klaR::stepclass(Sex ~ ., data = ref_lm,
                                        method = "lda",
                                        direction = lda_selvar,
                                        fold = nrow(ref_lm))
            ref_lm <- ref_lm[, c("Sex", best_lda$model$name)]
        }
        ## Build LDA model:
        mod_cv <- MASS::lda(Sex ~ ., data = ref_lm, CV = TRUE,
                            prior = c(0.5, 0.5))
        mod_pred <- MASS::lda(Sex ~ ., data = ref_lm,
                              prior = c(0.5, 0.5))
        ## Predict sex of target indiv:
        prediction <- predict(mod_pred, newdata = target)$posterior[, "M"]
        ## LOOCV results:
        cv_results <- dss_loocv(mod = mod_cv, ref = ref_lm, conf = conf,
                                method = "lda")
        ## LDA coefs:
        details <- coef(mod_pred)
    } else if (method == "rf") {
        ## 4.2. Random forests
        ## Tune random forest (parameter mtry):
        mtry_grid <- seq(from = 2, to = p, by = 1)
        errors <- sapply(mtry_grid, function(nbv) {
            rf <- randomForest(Sex ~ ., data = ref_lm,
                               ntree = rf_ntrees,
                               classwt = c(0.5, 0.5),
                               strata = ifelse(rf_downsampling, ref_lm$Sex, NULL),
                               sampsize = ifelse(rf_downsampling,
                                                 rep(min(table(ref_lm$Sex)), 2),
                                                 nrow(ref_lm)),
                               mtry = nbv)
            return(rf$err.rate[rf$ntree, "OOB"])
        })
        mtries <- data.frame(mtry = mtry_grid,
                             error = errors)
        best_mtry <- mtries[which.min(mtries$error), "mtry"]
        ## Build random forest with the best mtry parameter:
        mod <- randomForest(Sex ~ ., data = ref_lm,
                            ntree = rf_ntrees,
                            classwt = c(0.5, 0.5),
                            strata = ifelse(rf_downsampling, ref_lm$Sex, NULL),
                            sampsize = ifelse(rf_downsampling,
                                              rep(min(table(ref_lm$Sex)), 2),
                                              nrow(ref_lm)),
                            mtry = best_mtry)
        ## Make predictions and extract results:
        prediction <- predict(mod, newdata = target, type = "vote")[, "M"]
        cv_results <- dss_loocv(mod, ref = ref_lm, conf = conf,
                                method = "rf")
        details <- mod$importance[order(mod$importance, decreasing = TRUE), ]
        details <- data.frame(Score = details)
    } else if (method == "glmnet") {
        ## 4.3. Penalized logistic regression
        ## Choose the best lambda value by cross-validation:
        best_lambda <- glmnet::cv.glmnet(x = as.matrix(ref_lm[, -1]),
                                         y = ref_lm$Sex,
                                         family = "binomial",
                                         alpha = glmnet_type,
                                         nfolds = nrow(ref_lm),
                                         type.measure = glmnet_measure)
        ## Compute the corresponding model:
        glmmod <- glmnet::glmnet(x = as.matrix(ref_lm[, -1]),
                                 y = ref_lm$Sex,
                                 family = "binomial",
                                 alpha = glmnet_type, # 0 = ridge; 1 = lasso
                                 lambda = best_lambda$lambda.min)
        ## Compute the confusion matrix in LOOCV for this lambda value:
        cv_results <- dss_loocv(glmmod, ref = ref_lm, conf = conf,
                                method = "glmnet",
                                alpha = glmnet_type,
                                lambda = best_lambda$lambda.min)
        ## Make prediction for the target individual:
        prediction <- predict(glmmod,
                              newx = as.matrix(target),
                              type = "response")
        ## Return model coefs:
        details <- as.matrix(coef(glmmod))
    } else if (method == "linda") {
        ## 4.4. Robust linear discriminant analysis
        ## Build model:
        mod <- rrcov::Linda(x = ref_lm[, -1],
                            grouping = ref_lm$Sex,
                            prior = c(0.5, 0.5),
                            alpha = linda_alpha)
        ## Posterior proba for target individual:
        prediction <- predict(mod, target)@posterior[1, "M"]
        ## LDF coefs:
        details <- t(mod@ldf)
        ## LOOCV results:
        cv_results <- dss_loocv(mod, ref = ref_lm, conf = conf,
                                method = "linda",
                                linda_alpha = linda_alpha)
    }

    #######################
    ## 5. Return results ##
    #######################
    dtf_res["Model", 1] <- paste("Sex ~",
                                 paste0(colnames(ref_lm)[-1],
                                        collapse = " + "))
    dtf_res["% indeterminate (LOOCV)", 1] <- cv_results$indet_rate
    dtf_res["% accuracy (LOOCV)", 1] <- cv_results$accur_rate
    dtf_res["% accuracy for females (LOOCV)", 1] <- cv_results$accur_f
    dtf_res["% accuracy for males (LOOCV)", 1] <- cv_results$accur_m
    dtf_res["Prob(Sex==M)", 1] <- round(prediction, 3)
    dtf_res["Sex estimate", 1] <- dss_final_estimate(prob_m = prediction,
                                                     conf = conf)
    return(list(res_dss = dtf_res,
                table_loocv = cv_results$confusion_matrix,
                details = details))
}
