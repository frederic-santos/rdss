library(shiny)

dss_server <- function(input, output, session) {

  dss_env <- new.env() # app private envirnoment

###################################################
### 1. Read the dataset imported through the UI ###
###################################################
  observeEvent(input$button_load_data, {
    ## check that the user has indeed loaded a file:
    if (! is.null(input$data_file$name)) {
      ## TODO: ajouter vérification du fichier avant import
      dat <- read.table(file = input$data_file$datapath,
                        header = TRUE,
                        row.names = 1,
                        sep = input$field_sep,
                        dec = input$dec_sep,
                        na.strings = input$text_NA,
                        stringsAsFactors = TRUE)
      ## Extract TBD individuals:
      tbd <- dat[dat[, input$name_sex_column] == input$indic_tbd, ]
      ## Update list of TBD individuals:
      updateSelectInput(session,
                        inputId = "select_target_indiv",
                        choices = rownames(tbd))
    } else { # the user provided no data file
      showModal(modalDialog(title = "Error",
                            "Please select a file on your computer.",
                            easyClose = TRUE))
    }
    ## store data into the app environment:
    assign("dat", value = dat, envir = dss_env)
  })

########################################
### 2. Display the target individual ###
########################################
  ## Reactive expression for the whole dataset:
  dat <- reactive({
    if (input$button_load_data > 0 & exists("dat", envir = dss_env)) {
      get("dat", envir = dss_env)
    } else {
      return()
    }
  })

  ## Reactive expression for the TBD individual only:
  target <- reactive({
    return(dat()[input$select_target_indiv, ])
  })

  ## Render and display the selected target individual:
  output$target_indiv_values <- renderTable(target(),
                                            rownames = TRUE)

  ## Render a small text describing the target individual:
  output$text_description_target <- renderText({
    if (! is.null(target())) {
      paste("The individual",
            input$select_target_indiv,
            "has",
            ncol(target()[, !is.na(target()[1, ])]),
            "non-missing values out of the",
            ncol(dat()),
            "variables included in the dataset.")
    }
  })

}

### Local variables:
### ess-indent-level:2
### End:
