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
        "Upload Datasets"
      ),
      argonSidebarItem(
        tabName = "eda",
        icon = "chart-pie-35",
        icon_color = "warning",
        "Exploratory Data Analysis"
      ),
      argonSidebarItem(
        tabName = "pca",
        icon = "chart-bar-32",
        icon_color = "success",
        "Principle Component Analysis"
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
argonTabItems(
  ## Datasets ----
  argonTabItem(
    tabName = "data",
    argonTabSet(
      id = "tab-1",
      card_wrapper = TRUE,
      horizontal = TRUE,
      circle = FALSE,
      size = "sm",
      width = 12,
      iconList = NULL,
      
      ### Dataset ----
      argonTab(
        tabName = "Dataset",
        active = T,
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
            uiOutput("selectResponse"),
            argonColumn(
              center=T,
              actionButton("validate", "Validate Dataset")
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
      )
      
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
    
  ),
  
  
  ## PCA ----
  
  argonTabItem(
    tabName = "pca",
    argonTabSet(
      id = "tab-3",
      card_wrapper = TRUE,
      horizontal = TRUE,
      circle = FALSE,
      size = "sm",
      width = 12,
      iconList = NULL,
      
      ### Scree plot ----
      
      argonTab(
        tabName = "Scree plot",
        active = TRUE,
        argonRow(
          argonColumn(
            width = 3,
            selectInput(
              "choice",
              label = "Y Axis",
              choices = c("variance", "eigenvalue"),
              selected = "variance"
            )
          ),
          argonColumn(
            center = T,
            width = 9,
            plotOutput("screePlot", height = "100%") %>%
              withSpinner(
                color = "#5e72e4",
                type = 7,
                proxy.height = "600px"
              )
          )
        )
      ),
      
      ### Variable plot ----
      
      argonTab(
        tabName = "Variables",
        active = FALSE,
        argonRow(
          argonColumn(
            width = 3,
            selectInput(
              "axis_X",
              label = "X Axis Dimension",
              choices = seq(5),
              selected = 1
            ),
            
            uiOutput("secondSelector"),
            
            
            sliderInput(
              "n_cos2",
              label = "cos2:",
              min = 0.1,
              max = 0.9,
              value = 0.5,
              step = 0.1
            )
          ),
          argonColumn(
            center = T,
            width = 9,
            plotOutput("varPlot", height = "100%") %>%
              withSpinner(
                color = "#5e72e4",
                type = 7,
                proxy.height = "600px"
              )
          )
        )
      ),
      
      ### Bi-plot ----
      
      argonTab(
        tabName = "Biplot",
        active = FALSE,
        argonRow(
          argonColumn(
            width = 3,
            selectInput(
              "axis_X2",
              label = "X Axis Dimension",
              choices = seq(5),
              selected = 1
            ),
            
            uiOutput("secondSelector2")
            
          ),
          argonColumn(
            center = T,
            width = 9,
            plotOutput("biPlot", height = "100%") %>%
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
)

    ),

# Footer ----
footer = NULL
    )