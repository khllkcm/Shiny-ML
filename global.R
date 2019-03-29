library(shiny)
library(shinyjs)
library(shinycssloaders)
library(argonR)
library(argonDash)
library(DT)
library(plotly)
library(caret)
library(Deducer)
library(ROCR)
library(nnet)
library(glmnet)

better.summary.glm = function (x,
                               digits = max(3L, getOption("digits") - 3L),
                               symbolic.cor = x$symbolic.cor,
                               signif.stars = getOption("show.signif.stars"),
                               ...)
{
  if (length(x$aliased) == 0L) {
    cat("\nNo Coefficients\n")
  }
  else {
    df <- if ("df" %in% names(x))
      x[["df"]]
    else
      NULL
    if (!is.null(df) && (nsingular <- df[3L] - df[1L]))
      cat("Coefficients: (",
          nsingular,
          " not defined because of singularities)\n",
          sep = "")
    else
      cat("\nCoefficients:\n")
    coefs <- x$coefficients
    if (!is.null(aliased <- x$aliased) && any(aliased)) {
      cn <- names(aliased)
      coefs <- matrix(NA, length(aliased), 4L, dimnames = list(cn,
                                                               colnames(coefs)))
      coefs[!aliased, ] <- x$coefficients
    }
    printCoefmat(
      coefs,
      digits = digits,
      signif.stars = signif.stars,
      na.print = "NA",
      ...
    )
  }
  cat("\n\nDeviance Residuals: \n")
  if (x$df.residual > 5) {
    x$deviance.resid <- setNames(quantile(x$deviance.resid,
                                          na.rm = TRUE),
                                 c("Min", "1Q", "Median", "3Q", "Max"))
  }
  xx <- zapsmall(x$deviance.resid, digits + 1L)
  print.default(xx,
                digits = digits,
                na.print = "",
                print.gap = 2L)
  cat("\n", apply(
    cbind(
      paste(format(c(
        "Null",
        "Residual"
      ), justify = "right"), "deviance:"),
      format(unlist(x[c("null.deviance",
                        "deviance")]), digits = max(5L, digits + 1L)),
      " on",
      format(unlist(x[c("df.null", "df.residual")])),
      " degrees of freedom\n"
    ),
    1L,
    paste,
    collapse = " "
  ), sep = "")
  if (nzchar(mess <- naprint(x$na.action)))
    cat("  (", mess, ")\n", sep = "")
  cat("AIC: ", format(x$aic, digits = max(4L, digits + 1L)),
      "\n", sep = "")
  correl <- x$correlation
  if (!is.null(correl)) {
    p <- NCOL(correl)
    if (p > 1) {
      cat("\nCorrelation of Coefficients:\n")
      if (is.logical(symbolic.cor) && symbolic.cor) {
        print(symnum(correl, abbr.colnames = NULL))
      }
      else {
        correl <- format(round(correl, 2L),
                         nsmall = 2L,
                         digits = digits)
        correl[!lower.tri(correl)] <- ""
        print(correl[-1, -p, drop = FALSE], quote = FALSE)
      }
    }
  }
  cat("\n")
  invisible(x)
}



plotEval = function(model,
                    testData,
                    response,
                    type,
                    modeltype = "",
                    test) {
  predType = "response"
  if (modeltype == "NN")
    predType = "raw"
  testPred <-
    predict(model, testData, type = predType) %>% prediction(labels = test[[response]])
  switch (
    type,
    "ROC curve" =  return(
      performance(testPred, measure = "tpr", x.measure = "fpr") %>% plot()
    ),
    "Precision/recall graph" = return(
      performance(testPred, measure = "prec", x.measure = "rec") %>% plot()
    ),
    "Sensitivity/specificity plot" = return(
      performance(testPred, measure = "sens", x.measure = "spec") %>% plot()
    ),
    "Lift chart" = return(
      performance(testPred, measure = "lift", x.measure = "rpp") %>% plot()
    )
  )
}



confusion = function(model,
                     testData,
                     response,
                     type = "TPR/FPR",
                     modeltype = "",
                     test) {
  predType = "response"
  if (modeltype == "NN")
    predType = "raw"
  testProb = predict(model, testData, type = predType)
  testPred = prediction(testProb, labels = test[[response]])
  switch (type,
          "Matthews corr coeff" = {
            corr = performance(testPred, measure = "mat")
            threshold = corr@x.values[[1]][which.max(corr@y.values[[1]])]
          },
          "TPR/FPR" = {
            tpr = performance(testPred, measure = "tpr")
            fpr = performance(testPred, measure = "fpr")
            threshold = tpr@x.values[[1]][which.max(tpr@y.values[[1]] - fpr@y.values[[1]])]
          })
  offset = 0
  if ("2" %in% levels(test[[response]]))
    offset = 1
  if (modeltype == "NN")
    threshold = 0.5
  conf = confusionMatrix(test[[response]], factor(as.numeric(testProb >
                                                                  threshold)+offset,levels=c(0+offset,1+offset)))
  result = data.frame(c(conf$overall[1:2], conf$byClass))
  colnames(result) = modeltype
  return(result)
}
