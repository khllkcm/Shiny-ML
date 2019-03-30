server <- function(input, output, session) {
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
    datatable(df.dataForDisplay(), options = list(scrollX = T))
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
    updateSelectInput(
      session = session,
      inputId = "models",
      choices = c("Logit", "Probit", "NN", "LASSO Regression"),
      selected = NULL
    )
    genData()
  })
  
  genData = function() {
    yY = df.data()[, input$response]
    X = df.data()[, input$vars[-which(input$vars == input$response)]]
    df = cbind(yY, X)[!is.na(yY) & complete.cases(X), ]
    trainId = createDataPartition(
      df$yY,
      times = 1,
      p = 1 - (input$testprctg / 100),
      list = FALSE
    )
    colnames(df)[which(names(df) == "yY")] <- input$response
    df.train <<- df[trainId, ]
    df.test  <<- df[-trainId, ]
  }
  
  # Fit tabs ----
  observe({
    req(input$response)
    for (model in c("Logit", "Probit", "NN", "LASSO Regression")) {
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
      test = df.test
      if (model == "LASSO Regression") {
        test = model.matrix(as.formula(paste(
          input$response, " ~ ", paste(input$vars[-which(input$vars == input$response)], collapse = "+")
        )), df.test)[, -1]
      }
      df = cbind.data.frame(
        df,
        confusion(
          getModel(model),
          test,
          input$response,
          input$threshMetric,
          modeltype = model,
          df.test
        )
      )
    }
    return(
      datatable(
        t(df),
        class = "stripe",
        options = list(autoWidth = TRUE,
                       scrollX = T)
      ) %>% formatPercentage(colnames(t(df)), digits = 2)
      
    )
  })
  
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
    plotEval(
      model.logit(),
      df.test,
      input$response,
      input$plotType,
      test = df.test
    ),
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
    plotEval(
      model.probit(),
      df.test,
      input$response,
      input$plotType,
      test = df.test
    ),
    width = 600,
    height = 600
  )
  
  # NN ----
  model.nn = reactive({
    req(input$response)
    req(input$nnSize)
    nnet(
      as.formula(paste(
        input$response, " ~ ", paste(input$vars[-which(input$vars == input$response)], collapse = "+")
      )),
      data = df.train,
      family = binomial,
      size = input$nnSize,
      maxit = 1000
    )
  })
  
  output$nnroc = renderPlot(
    plotEval(
      model.nn(),
      df.test,
      input$response,
      input$plotType,
      "NN",
      test = df.test
    ),
    width = 600,
    height = 600
  )
  
  # Penalized ----
  
  model.lasso = reactive({
    x = model.matrix(as.formula(paste(
      input$response, " ~ ", paste(input$vars[-which(input$vars == input$response)], collapse = "+")
    )), df.train)[, -1]
    y = df.train[[input$response]]
    cv.lasso = cv.glmnet(x, y, family = "binomial", alpha = 1)
    glmnet(
      x,
      y,
      alpha = 1,
      family = "binomial",
      lambda = cv.lasso$lambda.min
    )
  })
  
  output$lasso = renderPrint(model.lasso()$beta)
  output$lassoroc = renderPlot(
    plotEval(
      model.lasso(),
      model.matrix(as.formula(paste(
        input$response, " ~ ", paste(input$vars[-which(input$vars == input$response)], collapse = "+")
      )), df.test)[, -1],
      input$response,
      input$plotType,
      test = df.test
    ),
    width = 600,
    height = 600
  )
  
  
  getModel = function(model) {
    switch (
      model,
      "Logit" = {
        return(model.logit())
      },
      "Probit" = {
        return(model.probit())
      },
      "NN" = {
        return(model.nn())
      },
      "LASSO Regression" = {
        return(model.lasso())
      }
    )
  }
  
  
  # Cross-validation ----
  
  df = data.frame(
    row.names =
      c("Accuracy",
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
  
  
  observeEvent(input$fitCross,{
    disable('fitCross')
    on.exit(enable('fitCross'))
    req(input$response)
    df = data.frame(
      row.names =
        c("Accuracy",
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
    for (model in input$crossModels) {
      df = cbind.data.frame(df,
                            crossConfusion(getCrossModel(model), df.test,
                                           input$response, model))
    }
    df <<- df
  })
  makeReactiveBinding('df')
  output$crossMatrix = renderDataTable({
    datatable(
      t(df),
      class = "stripe",
      options = list(autoWidth = TRUE,
                     scrollX = T)
    ) %>% formatPercentage(colnames(t(df)), digits = 2)
  })
  
  getCrossModel = function(model) {
    switch (
      model,
      "glm" = {
        return(train(
          as.formula(paste(
            input$response, " ~ ", paste(input$vars[-which(input$vars == input$response)], collapse = "+")
          )),
          data = df.train,
          method = "glm",
          trControl = trainControl(method = "cv",
                                   number = 10, verboseIter = T)
        ))
      },
      "Neural Network" = {
        return(train(
          as.formula(paste(
            input$response, " ~ ", paste(input$vars[-which(input$vars == input$response)], collapse = "+")
          )),
          data = df.train,
          method = "nnet",
          trace=F,
          tuneGrid = expand.grid(size=c(3,5),decay=c(0.01)),
          trControl = trainControl(method = "cv",
                                   number = 10, verboseIter = T)
        ))
      },
      "LASSO Regression" = {
        return(train(
          as.formula(paste(
            input$response, " ~ ", paste(input$vars[-which(input$vars == input$response)], collapse = "+")
          )),
          data = df.train,
          method = "glmnet",
          trControl = trainControl(method = "cv",
                                   number = 10, verboseIter = T)
        ))
      },
      "Random Forest" = {
        return(train(
          as.formula(paste(
            input$response, " ~ ", paste(input$vars[-which(input$vars == input$response)], collapse = "+")
          )),
          data = df.train,
          method = "rf",
          tuneGrid = data.frame(mtry=c(3,5)),
          trControl = trainControl(method = "cv",
                                   number = 10, verboseIter = T)
        ))
      },
      "SVM" = {
        return(train(
          as.formula(paste(
            input$response, " ~ ", paste(input$vars[-which(input$vars == input$response)], collapse = "+")
          )),
          data = df.train,
          method = "svmLinear",
          tuneGrid = data.frame(C=c(0.01,0.1,1)),
          trControl = trainControl(method = "cv",
                                   number = 10, verboseIter = T)
        ))
      }
      
    )
  }
}
