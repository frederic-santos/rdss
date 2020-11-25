ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "rdss: a GUI for sex estimation",
                  titleWidth = 325),

  ################
  ## 1. Sidebar ##
  ################
  dashboardSidebar(
    width = 325,
    tags$b("General instructions"),
    p("This user interface aims to facilitate the sex estimation",
      "for an unknown individual, using a reference sample composed",
      "of individuals of known sex."),
    p("To get a sex estimate, simply proceed through the following steps:"),
    sidebarMenu(id = "mainMenu",
                menuItem("1. Import data",
                         icon = icon("file-alt"),
                         tabName = "tab_data"),
                menuItem("2. Select the target individual",
                         icon = icon("bullseye"),
                         tabName = "tab_target_indiv"),
                menuItem("3. Check or customize the reference sample",
                         icon = icon("table"),
                         tabName = "tab_ref_sample"),
                menuItem("4. Perform sex estimation",
                         icon = icon("venus-mars"),
                         tabName = "tab_dss"),
                menuItem("5. Sensitivity analysis (optional)",
                         icon = icon("tools"),
                         tabName = "tab_sensitivity"),
                menuItem("Help",
                         icon = icon("question-circle"),
                         tabName = "tab_help")
                )
  ),

  #############
  ## 2. Tabs ##
  #############
  dashboardBody(
    tags$style(HTML("
      .progress-bar{
        background-color:#605ca8;
      }
      .box.box-info{
        border-top-color:#605ca8;
      }
      .box.box-solid.box-info > .box-header{
        background:#605ca8;
        background-color:#605ca8;
      }
      .box.box-solid.box-info{
        border:#666666;
      }
      .irs-bar {
        border-top: 1px solid #605ca8;
        border-bottom: 1px solid #605ca8;
        background: #605ca8;
      }
      .irs-bar-edge {
        border: 1px solid #605ca8;
        background: #605ca8;
      }
      .irs-single {
        background: #605ca8;
      }")),

    use_waiter(),
    tabItems(
      ## 2.1. Tab for data import:
      tabItem(tabName = "tab_data",
              h2("1. Import data"),
              fluidRow(
                box(title = "1.1. Data formatting",
                    width = 6,
                    height = 300,
                    solidHeader = FALSE,
                    collapsible = FALSE,
                    status = "info",
                    selectInput(inputId = "field_sep",
                                label = "Field separator",
                                choices = c("Comma (,)" = ",",
                                            "Semicolon (;)" = ";",
                                            "Whitespace" = " ",
                                            "Tabulation" = "\t"),
                                multiple = FALSE),
                    selectInput(inputId = "dec_sep",
                                label = "Decimal point",
                                choices = c("Comma (,)" = ",",
                                            "Dot (.)" = "."),
                                selected = ".",
                                multiple = FALSE),
                    textInput(inputId = "text_NA",
                              label = "Indicator for missing values",
                              placeholder = "Empty string by default",
                              value = "")
                    ),
                box(title = "1.2. Coding for the sex factor",
                    width = 6,
                    height = 300,
                    solidHeader = FALSE,
                    status = "info",
                    collapsible = FALSE,
                    textInput(inputId = "name_sex_column",
                              label = "Name of the column indicating the sex of the individuals in the data file",
                              value = "Sex"),
                    p("Indicate below the abreviations chosen for each sex,",
                      "i.e. the three levels of the sex factor in the data file."),
                    fluidRow(
                      column(4,
                             textInput(inputId = "indic_females",
                                       label = "Female individuals",
                                       value = "F"),
                             ),
                      column(4,
                             textInput(inputId = "indic_males",
                                       label = "Male individuals",
                                       value = "M"),
                             ),
                      column(4,
                             textInput(inputId = "indic_tbd",
                                       label = "Unknown individuals",
                                       value = "TBD")
                             )
                    ))
              ),
              fluidRow(
                box(title = "1.3. Load data",
                    width = 6,
                    height = 210,
                    solidHeader = FALSE,
                    collapsible = FALSE,
                    status = "info",
                    fileInput(inputId = "data_file",
                              label = "Select data file",
                              multiple = FALSE,
                              accept = c(".csv", ".txt")),
                    actionButton(inputId = "button_load_data",
                                 label = "Load dataset",
                                 icon = icon("file-upload")),
                    textOutput(outputId = "text_data_ok")
                    ),
                box(title = "Help & additional instructions",
                    width = 6,
                    status = "info",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    tags$ul(
                      tags$li("Only plain text files (CSV or TXT) are allowed."),
                      tags$li("Row names (in first column) are mandatory.",
                              "Please make sure that there are no duplicates."),
                      tags$li("Column names (i.e., headers, or variable names)",
                              "are also mandatory."),
                      tags$li("Individuals of known sex and individuals of",
                              "unknown sex must be placed in the same file."),
                      tags$li("An example data file is available",
                              a("here.", target = "_blank",
                                href = "https://gitlab.com/f-santos/rdss/-/blob/master/inst/poundbury_with_NA_reduced.csv"))
                      )
                    ))),

      ## 2.2. Tab for selecting the target individual:
      tabItem(tabName = "tab_target_indiv",
              h2("2. Select the target individual"),
              selectInput(inputId = "select_target_indiv",
                          label = "Select one target individual whose sex will be estimated",
                          width = 350,
                          choices = c("No data file loaded"),
                          multiple = FALSE),
              h4("Values of the selected individual"),
              div(style = "overflow:auto; width:100%;", tableOutput(outputId = "target_indiv_values")),
              h4("Overview"),
              textOutput(outputId = "text_description_target")
              ),

      ## 2.3. Tab for customization of the reference sample:
      tabItem(tabName = "tab_ref_sample",
              h2("3. Check or customize the reference sample"),
              h3("Reference sample for the current target individual"),
              tags$b(textOutput(outputId = "text_summary_ref")),
              br(),
              box(title = "Inspect the current reference dataset",
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  div(style = "overflow:auto; width:100%;", DT::DTOutput(outputId = "DT_ref_sample")),
                  downloadButton(outputId = "download_ref_sample",
                                 label = "Download reference sample (.csv)")
                  ),
              box(title = "Missing data in the reference dataset",
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  radioButtons(
                    inputId = "radio_md_ref",
                    label = "Type of plot",
                    choices = c("Missingness map" = "map",
                                "Patterns of missingness" = "pattern"),
                    selected = "map",
                    inline = TRUE
                    ),
                  fluidRow(
                    column(9,
                           plotOutput(outputId = "plot_md_ref",
                                      width = "100%")
                           ),
                    column(3,
                           tags$b("How to read this plot?"),
                           conditionalPanel(
                             condition = "input.radio_md_ref == 'pattern'",
                             p("Each row corresponds to one of the patterns of",
                               "missingness observed in the data.",
                               "Red is for missing values, blue for non-missing",
                               "values."),
                             p("The right column simply counts the missing values",
                               "appearing in each pattern.",
                               "The last row gives the total number of missing",
                               "values for each variable.",
                               "The left column gives the number of individuals",
                               "with each pattern."),
                             p("For more information, see the help page of",
                               tags$a(href = "https://stefvanbuuren.name/mice/reference/md.pattern.html",
                                      "mice::md.pattern()")),
                             checkboxInput(inputId = "checkbox_rotate_names",
                                           label = "Rotate variable names on plot",
                                           value = TRUE)
                           ),
                           conditionalPanel(
                             condition = "input.radio_md_ref == 'map'",
                             p("The percentage of missing values for each",
                               "variable is indicated along with its name."),
                             p("For more information, see the help page of",
                               tags$a(href = "http://visdat.njtierney.com/reference/vis_miss.html",
                                      "visdat::vis_miss()"))
                           ))),
                  downloadButton(outputId = "download_md_pattern",
                                 label = "Download missing data pattern (.png)")
                  ),
              h3("Subsetting/filtering criteria"),
              fluidRow(
                box(title = "Percentage of missing data for individuals",
                    width = 3,
                    status = "info",
                    solidHeader = FALSE,
                    collapsible = TRUE,
                    sliderInput(inputId = "perc_md_indiv",
                                label = "Maximal percentage of missing data allowed for individuals",
                                value = 100,
                                min = 0,
                                max = 100,
                                step = 5)
                    ),
                box(title = "Inclusion criterion for variables",
                    width = 3,
                    status = "info",
                    solidHeader = FALSE,
                    collapsible = TRUE,
                    sliderInput(inputId = "nb_min_indiv",
                                label = "Minimum number of individuals of each sex required for each variable",
                                value = 0,
                                min = 0,
                                max = 30,
                                step = 1)
                    ),
                box(title = "Percentage of missing data for variables",
                    width = 3,
                    status = "info",
                    solidHeader = FALSE,
                    collapsible = TRUE,
                    sliderInput(inputId = "perc_md_variables",
                                label = "Maximal percentage of missing data allowed for variables",
                                value = 100,
                                min = 0,
                                max = 100,
                                step = 5)
                    ),
                actionButton(inputId = "reload_ref",
                             label = "Cancel all criteria and reload data",
                             icon = icon("redo"))
              ),
              h4("History of filtering criteria"),
              tableOutput(outputId = "table_history")
              ),

      ## 2.4. DSS
      tabItem(tabName = "tab_dss",
              h2("4. Perform sex estimation"),
              box(title = "Analysis settings",
                  width = 12,
                  status = "info",
                  collapsible = TRUE,
                  fluidRow(
                    column(4,
                           selectInput(
                             inputId = "select_method_ML",
                             label = "Statistical method for sex estimation",
                             choices = c("Linear discriminant analysis" = "lda",
                                         "Penalized logistic regression" = "glmnet",
                                         "Random forest" = "rf",
                                         "Robust linear discriminant analysis" = "linda"),
                             selected = "lda",
                             multiple = FALSE)
                           ),
                    column(4,
                           sliderInput(
                             inputId = "slider_conf_level",
                             label = "Posterior prob. threshold for sex estimation",
                             value = 0.95,
                             min = 0.50, max = 0.95, step = 0.05)
                           ),
                    column(4,
                           radioButtons(
                             inputId = "radio_imputation_method",
                             label = "Method for missing data (single) imputation",
                             choices = c("Regularized iterative PCA" = "missMDA",
                                         "Random forests" = "missForest"),
                             inline = TRUE)
                           )
                  ),
                  conditionalPanel(
                    condition = "input.select_method_ML == 'lda'",
                    selectInput(inputId = "select_selvar_LDA",
                                label = "Variable selection",
                                choices = c("None" = "none",
                                            "Backward" = "backward",
                                            "Forward" = "forward",
                                            "Backward/forward" = "both"),
                                width = 150)
                  ),
                  conditionalPanel(
                    condition = "input.select_method_ML == 'rf'",
                    fluidRow(
                      column(4,
                             numericInput(inputId = "numeric_ntrees",
                                          label = "Number of trees",
                                          value = 1000,
                                          min = 100,
                                          max = 5000,
                                          step = 100)
                             ),
                      column(4,
                             checkboxInput(inputId = "checkbox_downsample_rf",
                                           label = "Downsample the majority class",
                                           value = FALSE)
                             ),
                      column(4,
                             helpText("Random forests do not work well when",
                                      "the classes (female / male) are",
                                      "strongly imbalanced. In this case,",
                                      "down-sampling the majority class is",
                                      "a simple workaround.")
                             )
                      )
                  ),
                  conditionalPanel(
                    condition = "input.select_method_ML == 'glmnet'",
                    fluidRow(
                      column(4,
                             radioButtons(inputId = "radio_glmnet_type",
                                          label = "Type of penalized regression",
                                          choices = c("Lasso" = "1",
                                                      "Ridge" = "0"),
                                          selected = "0",
                                          inline = TRUE)
                             ),
                      column(4,
                             radioButtons(inputId = "radio_glmnet_measure",
                                          label = "Criterion for optimaztion of lambda value",
                                          choices = c("Deviance" = "deviance",
                                                      "Misclassification error" = "class"),
                                          selected = "deviance",
                                          inline = TRUE)
                             )
                      )
                  ),
                  conditionalPanel(
                    condition = "input.select_method_ML == 'linda'",
                    fluidRow(
                      column(4,
                             sliderInput(inputId = "slider_linda_alpha",
                                         label = "Alpha value for MCD algorithm",
                                         min = 0.5,
                                         max = 0.95,
                                         value = 0.9,
                                         step = 0.01,
                                         width = 250)
                             ),
                      column(8,
                             helpText("Please note that the MCD algorithm",
                                      "requires having n >> p, i.e. the",
                                      "number of individuals must be much",
                                      "greater than the number of variables.",
                                      "In other cases, error messages will",
                                      "likely be displayed.")
                             )
                      )
                  ),
                  actionButton(inputId = "button_start_dss",
                               label = paste("Impute missing data and",
                                             "launch sex estimation"),
                               icon = icon("rocket"))
                  ),
              box(title = "Principal component analysis",
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  solidHeader = TRUE,
                  fluidRow(
                    column(3,
                           checkboxInput(
                             inputId = "checkbox_pca_names",
                             label = tags$b("Display individuals ID"),
                             value = FALSE)
                           ),
                    column(9,
                           selectInput(
                             inputId = "select_pca_ellipses",
                             label = paste("Display 95% data ellipses for groups",
                                           "(females / males)"),
                             choices = c("None" = "none",
                                         "Classical 95% data ellipses" = "classical",
                                         "Robust 95% data ellipses" = "robust"))
                           )
                  ),
                  plotOutput("plot_pca"),
                  downloadButton("download_pca_plot",
                                 label = "Download PCA plot (.png)")),
              box(title = "Sex estimation",
                  width = 12,
                  solidHeader = TRUE,
                  fluidRow(
                    column(8,
                           tags$b("Results for the target individual"),
                           tableOutput("table_dss"),
                           downloadButton(outputId = "download_dss_results",
                                 label = paste("Download sex estimation",
                                               "results (.csv)"))
                           ),
                    column(4,
                           tags$b("Confusion matrix for the reference dataset"),
                           tags$b("in LOOCV"),
                           tableOutput("table_loocv"),
                           br(),
                           tags$b(textOutput("text_details_ML")),
                           DT::DTOutput("table_details_ML")
                           )
                  ))
              ),

      ## 2.5. Sensitivity analysis
      tabItem(tabName = "tab_sensitivity",
              h2("5. Sensitivity analysis"),
              p("In this optional step, you can assess the impact of missing",
                "data imputation in the reference dataset.",
                "It is meaningless if your dataset is complete."),
              box(title = "Settings for multiple imputation",
                  width = 12,
                  status = "info",
                  collapsible = TRUE,
                  fluidRow(
                    column(8,
                           sliderInput(inputId = "slider_nb_mi",
                                       label = paste("Number of imputed",
                                                     "datasets to create",
                                                     "for sex estimation"),
                                       min = 5,
                                       max = 50,
                                       value = 10,
                                       step = 5)
                           ),
                    column(4,
                           helpText("Warning: for a large number of imputed",
                                    "datasets, the computation may be slow.")
                           )),
                  actionButton(inputId = "button_launch_mi",
                               label = "Launch multiple imputation",
                               icon = icon("coffee"))
                  ),
              box(title = "Multiple imputation principal component analysis (MIPCA)",
                  width = 12,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  solidHeader = TRUE,
                  plotOutput("plot_mipca"),
                  downloadButton("download_mipca_plot",
                                 label = "Download MIPCA plot (.png)")
                  ),
              box(title = "Sex estimation results for each imputed dataset",
                  width = 12,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  solidHeader = TRUE,
                  DT::DTOutput("table_sensitivity")
                  )
              ),

      ## 2.6. Help
      tabItem(tabName = "tab_help",
              h3("Quick reference and tips"),
              tags$ul(
                     tags$li("A",
                             a("video tutorial", target = "_blank",
                               href = ""),
                             "is available on Vimeo."),
                     tags$li("See the",
                             a("seminal paper by Murail et al. (1999)",
                               target = "_blank",
                               href = "https://doi.org/10.1002/(SICI)1099-1212(199901/02)9:1%3C39::AID-OA458%3E3.0.CO;2-V"),
                               "for more theoretical details."),
                     tags$li("This R package is currently under peer-review."),
                     tags$li("This R package will be updated on a regular basis.",
                             "To update to the latest development version, run",
                             code("remotes::install_git('https://gitlab.com/f-santos/rdss.git')"),
                             "into the R console.")
                   )
              ))
  ))

### Local variables:
### ess-indent-level:2
### End:
