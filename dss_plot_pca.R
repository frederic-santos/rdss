dss_plot_pca <- function(ref, imputed_ref, target, type,
                         ellipses = FALSE, labels = FALSE) {
### ref: dataframe, reference dataset (with missing values)
### imputed_ref: dataframe, imputed reference dataset
### target: 1-row dataframe, target individual
### type: string, either "result" or "sensitivity",
### ellipses: boolean (only relevant if type = "result")
### labels: boolean (only relevant if type = "result")

    if (is.null(imputed_ref)) {
        return()
    }

    ## Refactor some details in reference and target data:
    df <- merge_target_ref(target, ref)
    imp_df <- merge_target_ref(target, imputed_ref)

    ##############
    ## PCA plot ##
    ##############
    par(mfrow = c(1, 2))
    if (type == "result") {
        res_pca <- FactoMineR::PCA(imp_df, quali.sup = 1, graph = FALSE)
        FactoMineR::plot.PCA(res_pca, habillage = 1, choix = "ind",
                             invisible = "quali",
                             label = ifelse(labels, "ind", "none"),
                             graph.type = "classic",
                             title = paste("PCA of",
                                           rownames(target),
                                           "and the reference sample"),
                             col.hab = palette(c("red", "gray15", "gray70")))
        grid()
        ## Display a bigger point for the target indiv:
        points(x = res_pca$ind$coor[1, 1],
               y = res_pca$ind$coor[1, 2],
               col = "red", pch = 8, cex = 1.8)
        ## Add a 95% data ellipse for each group:
        if (ellipses == TRUE) {
            coor <- res_pca$ind$coor[-1, ]
            sex_ref <- factor(ref[, 1])
            car::dataEllipse(x = coor[, 1], # PC1
                             y = coor[, 2], # PC2
                             groups = sex_ref,
                             levels = 0.95, # 95% ellipse
                             add = TRUE,
                             col = c("gray15", "gray70"),
                             center.pch = "", plot.points = FALSE,
                             lwd = 1.1)
        }
        FactoMineR::plot.PCA(res_pca, choix = "var",
                             graph.type = "classic")
    } else { # type = "sensitivity"
        nb <- missMDA::estim_ncpPCA(df[, -1])
        mipca <- missMDA::MIPCA(df[, -1], ncp = nb$ncp, nboot = 100)
        plot.MIPCA(mipca, choice = "ind.supp",
                   graph.type = "classic", new.plot = FALSE)
        plot.MIPCA(mipca, choice = "var",
                   graph.type = "classic", new.plot = FALSE)
    }
}

merge_target_ref <- function(target, ref) {
    target[, 1] <- factor(target[, 1])
    levels(target[, 1]) <- paste("Target (", rownames(target), ")", sep = "")
    ref <- droplevels(ref)
    levels(ref[, 1]) <- c("Female", "Male")
    return(rbind(target[, colnames(ref)], ref))
}
