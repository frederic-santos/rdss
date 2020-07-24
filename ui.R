library(mice)
library(shiny)
library(shinydashboard)
library(DT)

ui_dss <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Sex estimation",
                  titleWidth = 325),

  ################
  ## 1. Sidebar ##
  ################
  dashboardSidebar(
    width = 325,
    tags$b("General instructions"),
    p("This user interface aims to facilitate the sex estimation",
      "for an unknown individual using a reference sample composed",
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
      }")),

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
                      tags$li("An example data file is available here.")
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
              tableOutput(outputId = "target_indiv_values"),
              h4("Overview"),
              textOutput(outputId = "text_description_target"),
              ),

      ## 2.3. Tab for data exploration and analysis settings:
      tabItem(tabName = "tab_ref_sample",
              h2("3. Check or customize the reference sample"),
              h3("Reference sample for the current target individual"),
              tags$b(textOutput(outputId = "text_summary_ref")),
              br(),
              DTOutput(outputId = "DT_ref_sample"),
              h3("Subsetting/filtering criteria"),
              fluidRow(
                box(title = "Settings",
                    width = 3,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    textInput("indivName",
                              label = "Name of the individual to be estimated",
                              value = "Indiv01")
                    ))),

      ## 2.5. Help
      tabItem(tabName = "tab_help",
              h3("Quick help and tips")
              ))
  ))

### Local variables:
### ess-indent-level:2
### End:
