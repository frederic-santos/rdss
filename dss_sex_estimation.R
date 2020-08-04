dss_sex_estimation <- function(ref, target, conf = 0.95,
                               nb_max_var = 3, nb_best_mod=5,
                               bias_red = TRUE) {
### ref : dataframe containing the reference dataset
### target: target individual
### conf : numeric value in ]0.5, 1[. Threshold pp for sex estimation
### nb_max_var : numeric value. Maximal number of variables allowed in LR models
### nb_best_mod : numeric value. Number of LR models retained
### bias_red : boolean. For LR models: apply Firth's correction or not

    ##################################
    ## 1. Prepare reference dataset ##
    ##################################
    colnames(ref)[1] <- "Sex"
    ref$Sex <- factor(ref$Sex)

    #################################################################
    ## 2. Set up the dataframe in which the results will be stored ##
    #################################################################
    dtf_res <- matrix(NA, nrow = nb_best_mod, ncol = 9)
    colnames(dtf_res) <- c("Model", "Nb_F_ref", "Nb_M_ref", "Nb_var",
                           "%indet_LOOCV", "%accuracy_LOOCV",
                           "max(coefs)", "Sex estimate", "Prob(Sex==M)")
    rownames(dtf_res) <- paste("Model", 1:nb_best_mod, sep = "_")
    dtf_res <- as.data.frame(dtf_res)

    ############################################
    ## 3. Store some constants in the results ##
    ############################################
    dtf_res[, "Nb_F_ref"] <- nrow(ref[ref$Sex == "F", ])
    dtf_res[, "Nb_M_ref"] <- nrow(ref[ref$Sex == "M", ])

    ###############################
    ## 4. Perform sex estimation ##
    ###############################
    if (bias_red == TRUE) { # apply Firth's correction
        mod <- logistf::logistf(Sex ~ ., data = ref)
        mod <- logistf::backward(mod)
    } else { # classical glm
        mod <- glm(Sex ~ ., data = ref, family = binomial)
        mod <- MASS::stepwise(mod, direction = "backward")
    }
    cv_results <- dss_loocv(mod, ref, conf = conf, br = bias_red)
    prediction <- predict(mod, newdata = target, type = "response")

    #######################
    ## 5. Return results ##
    #######################
    dtf_res[1, "Model"] <- paste("Sex ~",
                                     paste0(colnames(mod$data),
                                            collapse = TRUE, sep = "+"))
    dtf_res[1, "Nb_var"] <- ncol(ref) - 1
    dtf_res[1, "max(coefs)"] <- max(coef(mod))
    dtf_res[1, c("%indet_LOOCV", "%accuracy_LOOCV")] <- cv_results
    dtf_res[1, "Prob(Sex==M)"] <- round(prediction, 3)
    dtf_res[1, "Sex estimate"] <- dss_final_estimate(prob_m = prediction,
                                                     conf = conf)
}
