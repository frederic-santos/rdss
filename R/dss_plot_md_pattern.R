dss_plot_md_pattern <-
function(ref, target, type) {
### ref: dataframe, reference dataset
### target: 1-row dataframe, reference individual
### type: string, either "pattern" or "map"

    if (is.null(target)) {
      return()
    } else if (type == "pattern") {
      par(mar = c(1, 1, 1, 1))
      mice::md.pattern(x = ref[, -1], # without Sex factor
                       plot = TRUE)
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