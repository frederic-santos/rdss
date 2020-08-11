start_dss <- function() {
    ## Define the UI and server files for the app:
    app <- shiny::shinyApp(ui = ui, server = server)
    ## Run the app:
    shiny::runApp(app)
}
