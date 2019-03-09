server <- function(input, output) {
  ## Dataset ----
  
  output$selectResponse = renderUI(
    selectInput(
      inputId = "response",
      label = "Session:",
      choices = colnames(df.dataForDisplay()),
      selected = colnames(df.dataForDisplay())[1]
    )
  )
  df.dataForDisplay = reactive({
    req(input$file)
    validate(need(
      file_ext(input$file$name) %in% c(
        'text/csv',
        'text/comma-separated-values',
        'text/tab-separated-values',
        'text/plain',
        'csv',
        'tsv'
      ),
      "Wrong file format. Try again!"
    ))
    df <- read.csv(
      input$file$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote
    )
    if (input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  
  df.data = eventReactive(input$validate,{
    #return(read.csv(".csv"))
    validate(need(
      file_ext(input$file$name) %in% c(
        'text/csv',
        'text/comma-separated-values',
        'text/tab-separated-values',
        'text/plain',
        'csv',
        'tsv'
      ),
      "Wrong file format. Try again!"
    ))
    df = read.csv(
      input$file$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote
    )
    df[[input$response]] = as.factor(df[[input$response]])
    return(df)
  })
  
  ## Display Dataset ----
  output$contents <- renderDataTable({
    df.dataForDisplay()
  }, options = list(processing = FALSE))
  
  ## ANOVA ----
  output$selectAnovaVar = renderUI(selectInput(
    inputId = "anovaVar",
    label = "Variable: ",
    choices = names(Filter(is.numeric, df.data()))
  ))
  
  output$selectAnovaFactors = renderUI(
    selectInput(
      inputId = "anovaFactors",
      label = "Factor:",
      choices = names(Filter(is.factor, df.data())),
      multiple = TRUE
    )
  )
  
  anovaFactors = reactive({
    req(input$anovaFactors)
  })
  
  output$anova = renderPrint({
    summary(aov(as.formula(paste(
      input$anovaVar, " ~ ", paste(anovaFactors(), collapse = "*")
    )), data = df.data()))
  })
  
  ## Boxplot ----
  
  output$selectBoxplotVar = renderUI(selectInput(
    inputId = "boxplotVar",
    label = "Variable: ",
    choices = names(Filter(is.numeric, df.data()))
  ))
  
  output$selectBoxplotFactor = renderUI(selectInput(
    inputId = "boxplotFactor",
    label = "Factor:",
    choices = names(Filter(is.factor, df.data()))
  ))
  
  boxplotVar = reactive({
    req(input$boxplotVar)
  })
  
  boxplotFactor = reactive({
    req(input$boxplotFactor)
  })
  
  output$boxPlot <-  renderPlotly({
    plot_ly(
      data = df.data(),
      x = df.data()[[boxplotVar()]],
      color = df.data()[[boxplotFactor()]],
      colors = "RdYlBu",
      type = "box"
    )
  })
  
  ## PCA ----
  obj.pca = reactive({
    PCA(df.data()[,-(1:3)],graph = F)
  })
  
  ## Scree plot ----
  
  
  output$screePlot <- renderPlot({
    fviz_screeplot(obj.pca(), choice = input$choice)
  }, height = 600, width = 600)
  
  
  ## Variable plot ----
  
  output$secondSelector = renderUI(selectInput(
    "axis_Y",
    label = "Y Axis Dimension",
    choices = seq(5)[which(seq(5) != as.numeric(input$axis_X))],
    selected = 2
  ))
  
  Y_axis <- reactive({
    req(input$axis_Y)
  })
  
  output$varPlot =
    renderPlot({
      fviz_pca_var(obj.pca(),
                   col.var = "cos2",
                   axes = c(as.numeric(input$axis_X), as.numeric(Y_axis()))) +
        scale_color_gradient2(
          low = "white",
          mid = "blue",
          high = "red",
          midpoint = as.numeric(input$n_cos2),
          space = "Lab"
        ) + theme_light()
    }, height = 600, width = 600)
  
  ## Bi-Plot ----
  
  output$secondSelector2 = renderUI(selectInput(
    "axis_Y2",
    label = "Y Axis Dimension",
    choices = seq(5)[which(seq(5) != as.numeric(input$axis_X2))],
    selected = 2
  ))
  
  
  Y_axis2 <- reactive({
    req(input$axis_Y2)
  })
  
  
  output$biPlot = renderPlot({
    fviz_pca_biplot(
      obj.pca(),
      repel = T,
      alpha.var = "contrib",
      col.var = "cos2",
      col.ind = "#f5365c",
      axes = c(as.numeric(input$axis_X2), as.numeric(Y_axis2()))
    ) + theme_light()
  }, height = 600, width = 600)
  
}

