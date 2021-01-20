server <- function(input, output, session) {

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
      dtf <- dss_check_data(file = dtf,
                            sex = input$name_sex_column,
                            females = input$indic_females,
                            males = input$indic_males,
                            tbd = input$indic_tbd,
                            rm_empty_rows = input$checkbox_rm_empty_indiv)
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
        slider_max <- dss_min_fm(dtf = dtf)
        updateSliderInput(session = session,
                          inputId = "nb_min_indiv",
                          max = slider_max)
        ## store data into the app environment:
        assign("dtf", value = dtf, envir = dss_env)
        ## Add view button:
        output$button_view_data_file <- renderUI({
          actionButton(inputId = "view_data_file",
                       label = "View data file",
                       icon = icon("eye"))
        })
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

  ## Action button to view the whole dataset:
  observeEvent(input$view_data_file, {
    showModal(modalDialog(title = "View data",
                          div(style = "overflow:auto; width:100%;",
                              renderDataTable(dat())),
                          easyClose = TRUE))
  })

  ## Re-initialize all widgets of 3rd tab when
  ## changing target individual:
  observeEvent(input$select_target_indiv, {
    updateSliderInput(session,
                      inputId = "perc_md_indiv",
                      value = 100)
    updateSliderInput(session,
                      inputId = "perc_md_variables",
                      value = 100)
    updateSliderInput(session,
                      inputId = "nb_min_indiv",
                      value = 0)
    history$df <- NULL
  }, priority = 5)

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
    return(dat()[row_ref, col_ref, drop = FALSE])
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
    filename = paste("ref_data_for_", input$select_target_indiv, ".csv",
                     sep = ""),
    content = function(file) {
      write.csv(current$df, file)
    })

  ###################################################################
  ## Update current reference sample according to the 3 UI widgets ##
  ###################################################################
  observeEvent(input$perc_md_variables, {
    user_choice <- 1 - input$perc_md_variables / 100
    current$df <- remove_na(current$df,
                            which = "var",
                            prop_min = user_choice)
    history$df <- update_history(history$df,
                                 "Maximal %NA allowed for variables",
                                 input$perc_md_variables)
  }, ignoreInit = TRUE) # ignoreInit is important to avoid a crash here

  observeEvent(input$perc_md_indiv, {
    if (ncol(current$df) > 2) {
      user_choice <- 1 - input$perc_md_indiv / 100
      datfiltered <- remove_na(current$df[, -1],
                               which = "ind",
                               prop_min = user_choice)
      datfiltered <- data.frame(current$df[rownames(datfiltered), 1],
                                datfiltered)
      colnames(datfiltered)[1] <- input$name_sex_column
      current$df <- datfiltered
      history$df <- update_history(history$df,
                                   "Maximal %NA allowed for individuals",
                                   input$perc_md_indiv)
    } ## else, one single predictor: filtering does not make any sense
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
      fem <- current$df[current$df[, 1] == "F", ]
      mal <- current$df[current$df[, 1] == "M", ]
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
                       max = dss_min_fm(current$df))
  }, ignoreInit = TRUE, priority = -1)

  ################################################################
  ## Display pattern of missing values in the reference dataset ##
  ################################################################
  output$plot_md_ref <- renderPlot({
    dss_plot_md_pattern(ref = current$df,
                        target = target(),
                        type = input$radio_md_ref,
                        rotate = input$checkbox_rotate_names)
  })

  ## Download plot of md pattern:
  output$download_md_pattern <- downloadHandler(
    filename = paste("md_pattern_for_", input$select_target_indiv, ".png",
                     sep = ""),
    content = function(file) {
      if (input$radio_md_ref == "pattern") {
        png(file, width = 1000, height = 600)
        dss_plot_md_pattern(ref = current$df,
                            target = target(),
                            type = "pattern",
                            rotate = input$checkbox_rotate_names)
        dev.off()
      } else {
        ggsave(file,
               plot = dss_plot_md_pattern(ref = current$df,
                                          target = target(),
                                          type = "map"),
               height = 7, width = 10, units = "in",
               device = "png")
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

  ################################################################
  ## Forbid some estimation methods for one-variable dataframes ##
  ################################################################
  observeEvent(current$df, {
    if (ncol(current$df) <= 2) {
      updateSelectInput(session = session,
                        inputId = "select_method_ML",
                        choices = c("Linear discriminant analysis" = "lda"))
      updateSelectInput(session = session,
                        inputId = "select_selvar_LDA",
                        choices = c("None" = "none"))
    } else {
      updateSelectInput(session = session,
                        inputId = "select_method_ML",
                        choices = c("Linear discriminant analysis" = "lda",
                                    "Penalized logistic regression" = "glmnet",
                                    "Random forest" = "rf",
                                    "Robust linear discriminant analysis" = "linda"),
                        selected = "lda")
      updateSelectInput(session = session,
                        inputId = "select_selvar_LDA",
                        choices = c("None" = "none",
                                    "Backward" = "backward",
                                    "Forward" = "forward",
                                    "Backward/forward" = "both"))
      }
  })

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
    perc_na <- total_perc_missing(current$df)
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

  ## bonus: add View button for imputed dataset
  observeEvent(input$button_start_dss, {
    output$button_dl_imputed_ref <- renderUI({
      downloadButton(outputId = "dl_imputed_ref",
                     label = "Download imputed data (.csv)")
    })
  })
  output$dl_imputed_ref <- downloadHandler(
    filename = paste("imputed_data_for_", input$select_target_indiv, ".csv",
                     sep = ""),
    content = function(file) {
      write.csv(imputed_ref(), file)
    })

  ## 4.3. PCA:
  output$plot_pca <- renderPlot({
    dss_plot_pca(ref = current$df,
                 imputed_ref = imputed_ref(),
                 target = target(),
                 ellipses = input$select_pca_ellipses,
                 labels = input$checkbox_pca_names)
  })

  ## Download PCA:
  output$download_pca_plot <- downloadHandler(
    filename = paste("PCA_for_", input$select_target_indiv, ".png",
                     sep = ""),
    content = function(file) {
      png(file, width = 900, height = 450)
      dss_plot_pca(ref = current$df,
                 imputed_ref = imputed_ref(),
                 target = target(),
                 ellipses = input$select_pca_ellipses,
                 labels = input$checkbox_pca_names)
      dev.off()
    })

  ## 4.4. DSS:
  results_dss <- reactive({
    dss_sex_estimation(ref = imputed_ref(),
                       target = target(),
                       conf = as.numeric(input$slider_conf_level),
                       method = input$select_method_ML,
                       lda_selvar = input$select_selvar_LDA,
                       rf_ntrees = input$numeric_ntrees,
                       rf_downsampling = input$checkbox_downsample_rf,
                       glmnet_type = as.numeric(input$radio_glmnet_type),
                       glmnet_measure = input$radio_glmnet_measure,
                       linda_alpha = input$slider_linda_alpha)
  })
  output$table_loocv <- renderTable({
    results_dss()$table_loocv
  }, rownames = TRUE, colnames = TRUE)
  output$table_dss <- renderTable({
    results_dss()$res_dss
  }, rownames = TRUE, colnames = TRUE)

  ## Download table of DSS results:
  output$download_dss_results <- downloadHandler(
    filename = paste("sex_estimate_for_",
                     input$select_target_indiv,
                     ".csv", sep = ""),
    content = function(file) {
      write.csv(results_dss()$res_dss,
                file = file)
    })

  ## 4.5. Details about ML method:
  output$text_details_ML <- renderText({
    if (input$select_method_ML == "lda") {
      "Coefficients of LDA model"
    } else if (input$select_method_ML == "rf") {
      "Variable importance in random forest model"
    } else if (input$select_method_ML == "linda") {
      "LDF coefficients"
    } else if (input$select_method_ML == "glmnet") {
      "Variable coefficients"
    }
  })

  output$table_details_ML <- DT::renderDataTable({
    DT::datatable(results_dss()$details,
                  options = list(pageLength = 5))
  })

###############################
### 5. Sensitivity analysis ###
###############################
  ## 5.1. MIPCA plot:
  mipca <- eventReactive(input$button_launch_mi, {
    waiter_show(id = "plot_mipca")
    on.exit(waiter_hide(id = "plot_mipca"))
    if (total_perc_missing(current$df) == 0) {
      ## No imputation will be done if the reference dataset
      ## is already complete.
      return()
    } else { # there are NAs in the reference dataset
      imp <- merge_target_ref(target = target(), ref = current$df,
                              name_female = "F", name_male = "M")
      nb <- missMDA::estim_ncpPCA(imp[, -1])
      miboot <- missMDA::MIPCA(imp[, -1], ncp = nb$ncp,
                               method.mi = "Boot",
                               nboot = 100)
      mibayes <- missMDA::MIPCA(imp[, -1], ncp = nb$ncp,
                                method.mi = "Bayes",
                                nboot = input$slider_nb_mi)
      return(list(miboot = miboot, mibayes = mibayes,
                  refsex = factor(current$df[, 1])))
    }
  })

  output$plot_mipca <- renderPlot({
    dss_plot_mipca(mipca()$miboot)
  })

  ## Download PCA:
  output$download_mipca_plot <- downloadHandler(
    filename = paste("MIPCA_for_", input$select_target_indiv, ".png",
                     sep = ""),
    content = function(file) {
      png(file, width = 900, height = 450)
      dss_plot_mipca(mipca()$miboot)
      dev.off()
    })

  ## 5.2. Sensitivity on DSS results:
  output$table_sensitivity <- DT::renderDataTable({
    DT::datatable(dss_sensitivity(midata = mipca()$mibayes,
                                  conf = as.numeric(input$slider_conf_level),
                                  refsex = mipca()$refsex),
                  options = list(pageLength = 5))
  })
}

### Local variables:
### ess-indent-level:2
### End:
