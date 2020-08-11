dss_plot_mipca <-
function(midata) {
### midata: object return by MIPCA()

    if (is.null(midata)) {
        return()
    } else {
        par(mfrow = c(1, 2), cex = 1.05)
        plot.MIPCA(x = midata, choice = "ind.supp",
                   graph.type = "classic", new.plot = FALSE)
        plot.MIPCA(x = midata, choice = "var",
                   graph.type = "classic", new.plot = FALSE)
    }
}
