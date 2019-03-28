server <- function(input, output) {
  ## Dataset ----
  
  ### Active data tab ----
  
  output$tab <- renderText({
    " "
  })
  observeEvent(input$Next, {
    output$tab <- renderText({
      "  "
    })
  })
  observeEvent(input$prev, {
    output$tab <- renderText({
      " "
    })
  })
  
  
  df.dataForDisplay = reactive({
    if (is.null(input$file)) {
      # User has not uploaded a file yet
      return(data.frame())
    }
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
  
  df.data = eventReactive(input$validate, {
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
    df = df[input$vars]
    df[input$factors] <- lapply(df[input$factors], as.factor)
    return(df)
  })
  
  ## Display Dataset ----
  output$contents <- renderDataTable({
    datatable(df.dataForDisplay(),options = list(
      scrollX = T
    ))
  })
  
  ## Variables ----
  output$selectVars = renderUI(selectInput(
    "vars",
    "Variables to use:",
    colnames(df.dataForDisplay()),
    colnames(df.dataForDisplay()),
    multiple = TRUE
  ))
  
  output$selectFactors = renderUI(
    selectInput(
      "factors",
      "Variables of type factor:",
      input$vars,
      input$vars,
      multiple = TRUE
    )
  )
  
  
  
  output$selectResponse = renderUI(
    selectInput(
      inputId = "response",
      label = "Response:",
      choices = input$factors,
      selected = input$factors[1]
    )
  )
  
  
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
    req(input$anovaVar)
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
  
  ## Density ----
  
  output$selectDensityVar = renderUI(selectInput(
    inputId = "densityVar",
    label = "Variable:",
    choices = names(Filter(is.numeric, df.data()))
  ))
  
  output$density = renderPlot({
    req(input$densityVar)
    ggplot(df.data(), aes(x = eval(as.name(
      input$densityVar
    )))) + geom_density() + xlab(as.name(input$densityVar)) + theme_minimal()
  })
  
  
  # Test/Train Data ----
  df.train = NULL
  df.test = NULL
  
  observeEvent(input$validate, {
    y = df.data()[, input$response]
    X = df.data()[, input$vars[-which(input$vars == input$response)]]
    df = cbind(y, X)[!is.na(y) & complete.cases(X),]
    trainId = createDataPartition(
      df$y,
      times = 1,
      p = 1 - (input$testprctg / 100),
      list = FALSE
    )
    df.train <<- df[trainId,]
    df.test  <<- df[-trainId,]
  })
  
  
  # Fit tabs ----
  observe({
    for (model in c("Logit", "Probit", "NN", "Ridge Regression")) {
      if (model %in% input$models)
        showTab(inputId = "tabs", target = model)
      else
        hideTab(inputId = "tabs", target = model)
    }
  })
  
  # Output  ----
  output$matrix = renderDataTable({
    df = data.frame(
      row.names =
        c(
          "Accuracy",
          "Kappa",
          "Sensitivity",
          "Specificity",
          "Pos Pred Value",
          "Neg Pred Value",
          "Precision",
          "Recall",
          "F1",
          "Prevalence",
          "Detection Rate",
          "Detection Prevalence",
          "Balanced Accuracy"
        )
    )
    for (model in input$models) {
      df = cbind.data.frame(df,
                            confusion(
                              getModel(model),
                              df.test,
                              input$response,
                              input$threshMetric,
                              model
                            ))
    }
    return(
      datatable(
        t(df),
        class = "stripe",
        options = list(
          autoWidth = TRUE,
          scrollX = T
        )
      ) %>% formatPercentage(colnames(t(df)), digits = 2) 
      
    )
  })
  
  getModel = function(model) {
    switch (model,
            "Logit" = {
              return(model.logit())
            },
            "Probit" = {
              return(model.probit())
            })
  }
  
  
  # Logit ----
  model.logit = reactive({
    req(input$response)
    glm(as.formula(paste(
      input$response, " ~ ", paste(input$vars[-which(input$vars == input$response)], collapse = "+")
    )),
    data = df.train,
    family = binomial("logit"))
  })
  
  output$logit = renderPrint(better.summary.glm(summary(model.logit())))
  output$logitroc = renderPlot(
    plotEval(model.logit(), df.test, input$response, input$plotType),
    width = 600,
    height = 600
  )
  
  # Probit ----
  model.probit = reactive({
    req(input$response)
    glm(as.formula(paste(
      input$response, " ~ ", paste(input$vars[-which(input$vars == input$response)], collapse = "+")
    )),
    data = df.train,
    family = binomial("probit"))
  })
  
  output$probit = renderPrint(better.summary.glm(summary(model.probit())))
  output$probitroc = renderPlot(
    plotEval(model.probit(), df.test, input$response, input$plotType),
    width = 600,
    height = 600
  )
}
