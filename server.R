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
        ## First update of UI widget in 3rd tab:
        slider_max <- dss_min_fm(dtf = dtf,
                                 female = input$indic_females,
                                 male = input$indic_males)
        updateSliderInput(session = session,
                          inputId = "nb_min_indiv",
                          max = slider_max)
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
    row_ref <- rownames(dat())[dat()[, 1] != input$indic_tbd]
    ## (reminder: the 1st column is the Sex factor)
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
    datfiltered <- anthrostat::remove_na(current$df[, -1],
                                         which = "ind",
                                         prop_min = user_choice)
    datfiltered <- data.frame(current$df[rownames(datfiltered), 1],
                              datfiltered)
    colnames(datfiltered)[1] <- input$name_sex_column
    current$df <- datfiltered
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
      fem <- current$df[current$df[, 1] == input$indic_females, ]
      mal <- current$df[current$df[, 1] == input$indic_males, ]
      ## (reminder: the 1st column is the Sex factor)
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

  #######################
  ## Update UI element ##
  #######################
  observeEvent({input$select_target_indiv
    input$perc_md_indiv}, {
    updateNumericInput(session = session,
                       inputId = "nb_min_indiv",
                       max = dss_min_fm(current$df,
                                        input$indic_females,
                                        input$indic_males))
  }, ignoreInit = TRUE, priority = -1)

  ################################################################
  ## Display pattern of missing values in the reference dataset ##
  ################################################################
  output$plot_md_ref <- renderPlot({
    if (is.null(target())) {
      return()
    } else if (input$radio_md_ref == "pattern") {
      par(mar = c(1, 1, 1, 1))
      mice::md.pattern(x = current$df[, -1], # without Sex factor
                       plot = TRUE)
    } else if (input$radio_md_ref == "map") {
      par(mar = c(1, 1, 1, 1), cex = 1.3)
      mm <- visdat::vis_miss(current$df[, -1]) # without Sex factor
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
    updateSliderInput(session,
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
  ## 4.1. Update UI elements:
  observe({
    updateSliderInput(session = session,
                      inputId = "slider_nb_max_variables",
                      max = ncol(current$df) - 1,
                      value = ncol(current$df) - 1,
                      step = 1)
  })

  ## 4.2. Impute missing data:
  imputed_ref <- eventReactive(input$button_start_dss, {
    perc_na <- total_perc_missing(current$df, sex = input$name_sex_column)
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
    dss_impute_missing(current$df, method = input$radio_imputation_method)
  })

  ## 4.3. PCA:
  output$plot_pca <- renderPlot({
    dss_plot_pca(ref = current$df,
                 imputed_ref = imputed_ref(),
                 target = target(),
                 ellipses = input$radio_pca_ellipses,
                 labels = input$checkbox_pca_names)
  })

  ## 4.4. DSS:
  results_dss <- reactive({
    dss_sex_estimation(ref = imputed_ref(),
                       target = target(),
                       conf = input$radio_conf_level,
                       bias_red = input$checkbox_bias_LR)
  })
  output$table_loocv <- renderTable({
    results_dss()$table_loocv
  }, rownames = TRUE, colnames = TRUE)
  output$table_dss <- renderTable({
    results_dss()$res_dss
  }, rownames = TRUE, colnames = TRUE)
}

### Local variables:
### ess-indent-level:2
### End:
