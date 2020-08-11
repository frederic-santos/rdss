dss_sensitivity <-
function(midata, conf = 0.95, refsex) {
### midata: result from missMDA::MIPCA()
###         (contains both reference and target indivs)
### refsex: a factor, sex of reference individuals

    if (is.null(midata)) {
        return()
    } else {
        b <- length(midata$res.MI) # number of imputed datasets
        ## Initialize the dataframe of results:
        result <- matrix(NA, nrow = b, ncol = 5)
        colnames(result) <- c("Imputed dataset", "Sex estimate", "Prob(Sex==M)",
                              "% indeterminate (LOOCV)", "% accuracy (LOOCV)")
        result <- as.data.frame(result)
        result[, 1] <- 1:b

        ## Fill in this dataframe:
        for (k in 1:b) {
            reftemp <- data.frame(Sex = refsex,
                                  droplevels(midata$res.MI[[k]][-1, ]))
            temp <- dss_sex_estimation(target = midata$res.MI[[k]][1, ],
                                       ref = reftemp,
                                       conf = conf,
                                       method = "LDA")$res_dss
            result[k, "Sex estimate"] <- temp["Sex estimate", 1]
            result[k, "Prob(Sex==M)"] <- temp["Prob(Sex==M)", 1]
            result[k, "% indeterminate (LOOCV)"] <- temp["% indeterminate (LOOCV)", 1]
            result[k, "% accuracy (LOOCV)"] <- temp["% accuracy (LOOCV)", 1]
        }

        ## Return results:
        return(result)
    }
}
