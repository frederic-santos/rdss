dss_plot_pca <- function(ref, imputed_ref, target,
                         ellipses = "none", labels = FALSE) {
### ref: dataframe, reference dataset (with missing values)
### imputed_ref: dataframe, imputed reference dataset
### target: 1-row dataframe, target individual
### ellipses: string, either "none", "classical" or "robust"
### labels: boolean

    if (is.null(imputed_ref)) {
        return()
    }

    ## Refactor some details in reference and target data:
    imp_df <- merge_target_ref(target, imputed_ref)

    ##############
    ## PCA plot ##
    ##############
    par(mfrow = c(1, 2), cex = 1.08)
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
    if (ellipses != "none") {
        coor <- res_pca$ind$coor[-1, ]
        sex_ref <- factor(ref[, 1])
        car::dataEllipse(x = coor[, 1], # PC1
                         y = coor[, 2], # PC2
                         groups = sex_ref,
                         levels = 0.95, # 95% ellipse
                         add = TRUE,
                         robust = ifelse(ellipses == "robust", TRUE, FALSE),
                         col = c("gray15", "gray70"),
                         center.pch = "", plot.points = FALSE,
                         lwd = 1.1)
    }
    FactoMineR::plot.PCA(res_pca, choix = "var",
                         graph.type = "classic")
}

merge_target_ref <- function(target, ref,
                             name_female = "Female",
                             name_male = "Male") {
    target[, 1] <- factor(target[, 1])
    levels(target[, 1]) <- paste("Target (", rownames(target), ")", sep = "")
    ref <- droplevels(ref)
    levels(ref[, 1]) <- c(name_female, name_male)
    return(rbind(target[, colnames(ref)], ref))
}
