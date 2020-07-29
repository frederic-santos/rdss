dss_server <- function(input, output, session) {

  dss_env <- new.env() # app private envirnoment

###################################################
### 1. Read the dataset imported through the UI ###
###################################################
  observeEvent(input$button_load_data, {
    ## Check that the user has indeed loaded a file:
    if (! is.null(input$data_file$name)) {
      ## Import file (without knowing yet whether it is valid):
      dtf <- read.table(file = input$data_file$datapath,
                        header = TRUE,
                        sep = input$field_sep,
                        dec = input$dec_sep,
                        na.strings = input$text_NA,
                        stringsAsFactors = TRUE)
      ## Check whether the file is valid:
      dtf <- check_data_dss(file = dtf,
                            sex = input$name_sex_column,
                            females = input$indic_females,
                            males = input$indic_males,
                            tbd = input$indic_tbd)
      ## If the file is valid:
      if (! is.null(dtf)) {
        ## Extract TBD individuals:
        tbd <- dtf[dtf[, input$name_sex_column] == input$indic_tbd, ]
        ## Update list of TBD individuals:
        updateSelectInput(session,
                          inputId = "select_target_indiv",
                          choices = rownames(tbd))
        ## Return a message to say OK:
        output$text_data_ok <- renderText("Data file loaded successfully!")
        ## TODO : à améliorer (et sans doute déplacer) :
        ## Update widget for minimal number of indiv:
        updateNumericInput(session,
                           inputId = "nb_min_indiv",
                           max = nrow(dtf) - nrow(tbd))
        ## store data into the app environment:
        assign("dtf", value = dtf, envir = dss_env)
      }
    } else { # the user provided no data file
      showModal(modalDialog(title = "Error",
                            "Please select a file on your computer.",
                            easyClose = TRUE))
    }
  })

########################################
### 2. Display the target individual ###
########################################
  ## Reactive expression for the whole dataset:
  dat <- reactive({
    if (input$button_load_data > 0 & exists("dtf", envir = dss_env)) {
      get("dtf", envir = dss_env)
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

###############################################
### 3. Constitution of the reference sample ###
###############################################
  ## Create whole reference sample for the current target indiv:
  ref <- reactive({
    row_ref <- rownames(dat())[dat()[, input$name_sex_column] != input$indic_tbd]
    col_ref <- colnames(dat())[!is.na(target()[1, ])]
    return(dat()[row_ref, col_ref])
  })

  ## Current reference sample
  ## (filtered from ref(), using the UI-defined criteria):
  current <- reactiveValues(
    df = NULL # fake init to NULL...
  )
  observeEvent(target(), {
    current$df <- ref() # ... and instantly set to ref() after data loading
  })

  ## Download ref sample:
  output$download_ref_sample <- downloadHandler(
    filename = paste("ref_data_", input$select_target_indiv, ".csv", sep = ""),
    content = function(file) {
      write.csv(current$df, file)
    })

  ###################################################################
  ## Update current reference sample according to the 3 UI widgets ##
  ###################################################################
  observeEvent(input$perc_md_variables, {
    user_choice <- 1 - input$perc_md_variables / 100
    current$df <- anthrostat::remove_na(current$df,
                                        which = "var",
                                        prop_min = user_choice)
    history$df <- update_history(history$df,
                                 "Maximal %NA allowed for variables",
                                 input$perc_md_variables)
  }, ignoreInit = TRUE) # ignoreInit is important to avoid a crash here

  observeEvent(input$perc_md_indiv, {
    user_choice <- 1 - input$perc_md_indiv / 100
    current$df <- anthrostat::remove_na(current$df,
                                        which = "ind",
                                        prop_min = user_choice)
    history$df <- update_history(history$df,
                                 "Maximal %NA allowed for individuals",
                                 input$perc_md_indiv)
  }, ignoreInit = TRUE) # ignoreInit is important to avoid a crash here

  observeEvent(input$nb_min_indiv, {
    sex_count <- aggregate(current$df,
                           by = list(current$df[, input$name_sex_column]),
                           FUN = function(x) return(sum(!is.na(x))))[, -1]
    min_per_var <- apply(sex_count, MARGIN = 2, FUN = min)
    current$df <- current$df[, min_per_var >= input$nb_min_indiv]
    history$df <- update_history(history$df,
                                 "Required number of individuals for each sex",
                                 input$nb_min_indiv)
  }, ignoreInit = TRUE) # ignoreInit is important to avoid a crash here

  #############################################
  ## Display reference sample in a DataTable ##
  #############################################
  output$DT_ref_sample <- DT::renderDataTable(
    DT::datatable(current$df, options = list(pageLength = 5))
    )

  #####################################################
  ## Display (text) summary for the reference sample ##
  #####################################################
  output$text_summary_ref <- renderText({
    if (! is.null(target())) {
      fem <- current$df[current$df[, input$name_sex_column] == input$indic_females, ]
      mal <- current$df[current$df[, input$name_sex_column] == input$indic_males, ]
      paste("The reference sample has currently ",
            nrow(fem),
            " female individuals (",
            nrow(na.omit(fem)),
            " of them have no missing values), and ",
            nrow(mal),
            " male individuals (",
            nrow(na.omit(mal)),
            " of them have no missing values).",
            sep = "")
    }
  })
  ## output$text_nb_md_ref <- renderText({
  ##   if (! is.null(target())) {
  ##     paste("In total, ",
  ##           total_perc_missing(current$df, input$name_sex_column),
  ##           "% of data cells are missing. ",
  ##           "Patterns of missing data are as follows:",
  ##           sep = "")
  ##   }
  ## })

  ################################################################
  ## Display pattern of missing values in the reference dataset ##
  ################################################################
  output$plot_md_ref <- renderPlot({
    if (is.null(target())) {
      return()
    } else if (input$radio_md_ref == "pattern") {
      par(mar = c(1, 1, 1, 1))
      dat_wt_sex <- current$df[, colnames(current$df) != input$name_sex_column]
      mice::md.pattern(x = dat_wt_sex,
                       plot = TRUE)
    } else if (input$radio_md_ref == "map") {
      par(mar = c(1, 1, 1, 1), cex = 1.3)
      dat_wt_sex <- current$df[, colnames(current$df) != input$name_sex_column]
      mm <- visdat::vis_miss(dat_wt_sex)
      mm +
        theme(legend.text = element_text(size = 12),
              axis.text.x = element_text(size = 10),
              axis.text.y = element_text(size = 12),
              axis.title.y = element_text(size = 12))
    }
  })

  ###################################################################
  ## Reload whole reference sample if `reload' button is triggered ##
  ## (and reset all UI widgets in 3rd tab)                         ##
  ###################################################################
  observeEvent(input$reload_ref, {
    updateSliderInput(session,
                      inputId = "perc_md_variables",
                      value = 100)
    updateSliderInput(session,
                      inputId = "perc_md_indiv",
                      value = 100)
    updateNumericInput(session,
                       inputId = "nb_min_indiv",
                       value = 0)
    current$df <- ref()
    history$df <- NULL
  })

  #############
  ## History ##
  #############
  history <- reactiveValues(
    df = NULL
  )
  output$table_history <- renderTable(history$df,
                                      rownames = 1)

#################################
### 4. Perform sex estimation ###
#################################
  ## 4.1. Impute missing data:
  imputed_ref <- eventReactive(input$button_start_dss, {
    perc_na <- total_perc_missing(current$df, input$name_sex_column)
    if (perc_na >= 50) {
      showModal(modalDialog(title = "Too many missing values",
                            paste("There is more than 50% of missing data",
                                  "cells in the current reference sample.",
                                  "Sex estimation cannot be reliable in this",
                                  "context. Go back to the previous tab and",
                                  "try to lower this percentage."),
                            easyClose = TRUE))
      return()
    } else if (perc_na >= 35) {
      showModal(modalDialog(title = "High percentage of missing values",
                            paste("There is more than 35% of missing data",
                                  "cells in the current reference sample.",
                                  "Sex estimation will be performed, but",
                                  "it might not be reliable."),
                            easyClose = TRUE))
    }
    missMDA::imputePCA(X = current$df)
  })
  
  ## 4.2. PCA:
  output$plot_pca <- renderPlot({
    dss_plot_pca(ref = current$df, target = target(),
                 ellipses = input$checkbox_pca_ellipses,
                 labels = input$checkbox_pca_names,
                 sex = input$name_sex_column)
  })
}

### Local variables:
### ess-indent-level:2
### End:
