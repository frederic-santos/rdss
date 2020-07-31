dss_plot_pca <- function(ref = ref, target = target,
                         ellipses = TRUE, labels = FALSE,
                         sex = "Sex") {

    if (is.null(ref)) {
        return()
    }

    ## Refactor some details in reference and target data:
    target[, sex] <- factor(target[, sex])
    levels(target[, sex]) <- paste("Target (", rownames(target), ")", sep = "")
    ref <- droplevels(ref)
    levels(ref[, sex]) <- c("Female", "Male")
    df <- merge_target_ref(target, ref)

    #################
    ## Compute PCA ##
    #################
    res_pca <- FactoMineR::PCA(df, quali.sup = 1, graph = FALSE)

    ########################
    ## PCA of individuals ##
    ########################
    par(mfrow = c(1, 2))
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
    if (ellipses) {
        coor <- res_pca$ind$coor[-1, ]
        sex_ref <- factor(ref[, sex])
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
}

merge_target_ref <- function(target, ref) {
    return(rbind(target[, colnames(ref)], ref))
}
