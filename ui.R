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
        "Data"
      ),
      argonSidebarItem(
        tabName = "eda",
        icon = "chart-pie-35",
        icon_color = "warning",
        "Exploratory Data Analysis"
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
        $('li[class*=\\'active\\']').removeClass('active');
        Shiny.onInputChange('currentTab', $('.active').data().value);
        });"
)
    ),
useShinyalert(),
useShinyjs(),
argonTabItems(
  ## Datasets ----
  argonTabItem(
    tabName = "data",
    argonColumn(
      conditionalPanel("output.tab==' '",
    argonCard(
      title = "Upload Dataset",
      width = 12,
        argonRow(
          argonColumn(
            width = 3,
            argonColumn(
              fileInput(
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
              )
            ),
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
            argonColumn(
              center=T,
              actionButton("Next", "Next")
            )
          ),
          argonColumn(
            center = T,
            width = 9,
            div(
              style = 'overflow-x: scroll',
              dataTableOutput("contents") %>%
                withSpinner(
                  color = "#5e72e4",
                  type = 7,
                  proxy.height = "400px"
                )
            )
          )
        )
    )),
    conditionalPanel("output.tab=='  '",
                     argonCard(width=12,
                               title = "Choose Variables",
                               argonColumn(
                                 center=T,
                                 argonRow(center = T,
                                 uiOutput("selectVars"),
                                 tags$div(
                                   style="margin-left:50px;margin-right:50px;",
                                   uiOutput("selectFactors")
                                 ),
                                 uiOutput("selectResponse")),
                                 actionButton("prev", "Previous"),
                                 actionButton("validate", "Validate")
                               )))
      
    )
  ),
  
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
    
  )
)

    ),

# Footer ----
footer = verbatimTextOutput("tab")
    )