ui <- argonDashPage(
  title = "ML Dashboard",
  author = "Khalil",
  description = "ML-Shiny",
  
  # Sidebar ----
  
  sidebar = argonDashSidebar(
    skin = "light",
    background = "white",
    size = "md",
    side = "left",
    id = "sidebar",
    dropdownMenus =
      argonSidebarHeader(title = "Main Menu"),
    argonSidebarMenu(
      argonSidebarItem(
        tabName = "data",
        icon = "single-copy-04",
        icon_color = "default",
        "Upload Data"
      ),
      argonSidebarItem(
        tabName = "eda",
        icon = "chart-pie-35",
        icon_color = "warning",
        "Exploratory Data Analysis"
      ),
      argonSidebarItem(
        tabName = "fit",
        icon = "atom",
        icon_color = "info",
        "Fit Models on Training Set"
      ),
      argonSidebarItem(
        tabName = "cross",
        icon = "books",
        icon_color = "success",
        "10-fold Cross-Validation"
      )
    )
  ),
  
  # Navbar ----
  
  navbar = NULL,
  
  # Header ----
  
  
  header = argonDashHeader(
    gradient = TRUE,
    color = "primary",
    separator = TRUE,
    separator_color = "secondary"
  ),
  
  # Body ----
  
  body = argonDashBody(
    ## CSS ----
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css",
                href = "style.css"),
      ## JS ----
      tags$script(
        "$(document).on('click', function(event) {
        $('li[class*=\\'active\\']').find('a').addClass('active show');
        $('li[class*=\\'active\\']').removeClass('active');
        Shiny.onInputChange('currentTab', $('.active').data().value);
        });"
)
    ),
useShinyjs(),
argonTabItems(
  ## Datasets ----
  argonTabItem(tabName = "data",
               argonColumn(
                 conditionalPanel(
                   "output.tab==' '",
                   argonCard(
                     title = "Upload Dataset",
                     width = 12,
                     argonRow(
                       argonColumn(
                         width = 3,
                         argonColumn(fileInput(
                           "file",
                           "Choose CSV File",
                           multiple = TRUE,
                           accept = c(
                             'text/csv',
                             'text/comma-separated-values',
                             'text/tab-separated-values',
                             'text/plain',
                             'csv',
                             'tsv'
                           )
                         )),
                         argonRow(
                           argonColumn(
                             width = 6,
                             radioButtons(
                               "sep",
                               "Separator",
                               choices = c(
                                 Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"
                               ),
                               selected = ","
                             )
                           ),
                           argonColumn(
                             width = 6,
                             radioButtons(
                               "quote",
                               "Quote",
                               choices = c(
                                 None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"
                               ),
                               selected = '"'
                             )
                           )
                         ),
                         
                         argonRow(
                           argonColumn(width = 6,
                                       checkboxInput("header", "Header", TRUE)),
                           argonColumn(
                             width = 6,
                             radioButtons(
                               "disp",
                               "Display",
                               choices = c(Head = "head",
                                           All = "all"),
                               selected = "head"
                             )
                           )
                         ),
                         argonColumn(center = T,
                                     actionButton("Next", "Next"))
                       ),
                       argonColumn(
                         center = T,
                         width = 9,
                         dataTableOutput("contents") %>%
                           withSpinner(
                             color = "#5e72e4",
                             type = 7,
                             proxy.height = "400px"
                           )
                       )
                     )
                   )
                 ),
                 conditionalPanel(
                   "output.tab=='  '",
                   argonCard(
                     width = 12,
                     title = "Choose Variables",
                     argonColumn(
                       center = T,
                       argonRow(
                         center = T,
                         uiOutput("selectVars"),
                         tags$div(style = "margin-left:50px;margin-right:50px;",
                                  uiOutput("selectFactors")),
                         uiOutput("selectResponse")
                       ),
                       argonRow(
                         center = T,
                         sliderInput(
                           'testprctg',
                           label = 'Test Set Percentage:',
                           min = 20,
                           max = 90,
                           step = 1,
                           value = 25
                         )
                       ),
                       actionButton("prev", "Previous"),
                       actionButton("validate", "Validate")
                     )
                   )
                 )
                 
               )),
  
  ## EDA ----
  argonTabItem(
    tabName = "eda",
    argonTabSet(
      id = "tab-2",
      card_wrapper = TRUE,
      horizontal = TRUE,
      circle = FALSE,
      size = "sm",
      width = 12,
      iconList = NULL,
      
      ### Boxplot ----
      
      argonTab(
        tabName = "Boxplot",
        active = TRUE,
        argonRow(
          argonColumn(
            width = 3,
            uiOutput("selectBoxplotVar"),
            uiOutput("selectBoxplotFactor")
          ),
          argonColumn(
            center = T,
            width = 9,
            plotlyOutput("boxPlot", height = "100%") %>%
              withSpinner(
                color = "#5e72e4",
                type = 7,
                proxy.height = "400px"
              )
          )
        )
      ),
      
      ### Density ----
      argonTab(
        tabName = "Distribution",
        active = FALSE,
        argonRow(
          argonColumn(width = 3,
                      uiOutput("selectDensityVar")),
          argonColumn(
            center = T,
            width = 9,
            plotOutput("density") %>%
              withSpinner(
                color = "#5e72e4",
                type = 7,
                proxy.height = "400px"
              )
          )
        )
      ),
      
      ### ANOVA ----
      argonTab(
        tabName = "ANOVA",
        active = FALSE,
        argonRow(
          argonColumn(
            width = 3,
            uiOutput("selectAnovaVar"),
            uiOutput("selectAnovaFactors")
          ),
          argonColumn(
            center = T,
            width = 9,
            verbatimTextOutput("anova") %>%
              withSpinner(
                color = "#5e72e4",
                type = 7,
                proxy.height = "400px"
              )
          )
        )
      )
      
    )
    
  ),
  
  # Fit models ----
  argonTabItem(
    tabName = "fit",
    active = FALSE,
    argonCard(width = 12,
              argonRow(
                argonColumn(
                  width = 2,
                  selectInput(
                    inputId = "models",
                    label = "Select models:",
                    choices = c("Logit", "Probit", "NN", "LASSO Regression"),
                    multiple = T
                  ),
                  selectInput(
                    inputId = "threshMetric",
                    label = "Threshold Metric:",
                    choices = c("Matthews corr coeff",
                                "TPR/FPR")
                  ),
                  numericInput(
                    inputId = "nnSize",
                    label = "Neural Network Size:",
                    value = 3,
                    min = 1,
                    max = 10
                  ),
                  selectInput(
                    inputId = "plotType",
                    label = "Evaluation Plot:",
                    choices = c(
                      "ROC curve",
                      "Precision/recall graph",
                      "Sensitivity/specificity plot",
                      "Lift chart"
                    )
                  )
                ),
                argonColumn(
                  center = T,
                  width = 10,
                  tabsetPanel(
                    id = "tabs",
                    tabPanel("Output", dataTableOutput("matrix")),
                    tabPanel(
                      "Logit",
                      argonTabSet(
                        width = 12,
                        id = "logitOutput",
                        argonTab(
                          tabName = "Logit Summary",
                          active = T,
                          verbatimTextOutput("logit")
                        ),
                        argonTab(
                          tabName = "Logit Evaluation Plot",
                          plotOutput("logitroc", height = "100%") %>%
                            withSpinner(
                              color = "#5e72e4",
                              type = 7,
                              proxy.height = "600px"
                            )
                        )
                      )
                    ),
                    tabPanel(
                      "Probit",
                      argonTabSet(
                        width = 12,
                        id = "probitOutput",
                        argonTab(
                          tabName = "Probit Summary",
                          active = T,
                          verbatimTextOutput("probit")
                        ),
                        argonTab(
                          tabName = "Probit Evaluation Plot",
                          plotOutput("probitroc", height = "100%") %>%
                            withSpinner(
                              color = "#5e72e4",
                              type = 7,
                              proxy.height = "600px"
                            )
                        )
                      )
                    ),
                    tabPanel("NN", argonTabSet(
                      width = 12,
                      id = "nnoutput",
                      argonTab(
                        tabName = "NN Evaluation Plot",
                        active = T,
                        plotOutput("nnroc", height = "100%") %>%
                          withSpinner(
                            color = "#5e72e4",
                            type = 7,
                            proxy.height = "600px"
                          )
                      )
                    )),
                    tabPanel(
                      "LASSO Regression",
                      argonTabSet(
                        width = 12,
                        id = "lassoOutput",
                        argonTab(
                          tabName = "LASSO Summary",
                          active = T,
                          verbatimTextOutput("lasso")
                        ),
                        argonTab(
                          tabName = "LASSO Evaluation Plot",
                          plotOutput("lassoroc", height = "100%") %>%
                            withSpinner(
                              color = "#5e72e4",
                              type = 7,
                              proxy.height = "600px"
                            )
                        )
                      )
                    )
                  )
                )
              ))
  ),
  
  # Cross Validation ----
  argonTabItem(
    tabName = "cross",
    active = FALSE,
    argonCard(width = 12,
              argonRow(
                argonColumn(
                  width = 2,
                  center = T,
                  selectInput(
                    inputId = "crossModels",
                    label = "Select models:",
                    choices = c("glm", "Neural Network", "LASSO Regression", "Random Forest", "SVM"),
                    multiple = T
                  ),
                  actionButton("fitCross","Fit models")
                ),
                argonColumn(
                  center = T,
                  width = 10,
                  dataTableOutput("crossMatrix") %>%
                    withSpinner(
                      color = "#5e72e4",
                      type = 7,
                      proxy.height = "600px"
                    )
                )
              ))
  )
)
    ),

# Footer ----
footer = verbatimTextOutput("tab")
  )