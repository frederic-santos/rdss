dss_plot_md_pattern <-
function(ref, target, type, rotate = TRUE) {
### ref: dataframe, reference dataset
### target: 1-row dataframe, reference individual
### type: string, either "pattern" or "map"
### rotate: boolean, passed to md.pattern() for its rotate.names argument

    if (is.null(target)) {
        return()
    } else if (ncol(ref) <= 2) {
        plot(x = 0, y = 0, pch = "",
             xlim = c(-5, 5), ylim = c(-5, 5))
        text(x = 0, y = 0,
             col = "red",
             labels = "Error: At least two metric variables are needed for this plot.")
    } else if (type == "pattern") {
        par(mar = c(1, 1, ifelse(rotate, 0, 1), 1))
        mice::md.pattern(x = ref[, -1], # without Sex factor
                         plot = TRUE,
                         rotate.names = rotate)
    } else if (type == "map") {
        par(mar = c(1, 1, 1, 1), cex = 1.3)
        mm <- visdat::vis_miss(ref[, -1]) # without Sex factor
        mm +
            theme(legend.text = element_text(size = 12),
                  axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 12),
                  axis.title.y = element_text(size = 12))
    }
}
