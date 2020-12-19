.bainCleanModelInput <- function(input) {
  return(gsub("\n+", ";", input))
}

.bainGetCitations <- function() {
  citations <- c(
    "Gu, X., Mulder, J., and Hoijtink, H. (2017). Approximate adjusted fractional Bayes factors: A general method for testing informative hypotheses. British Journal of Mathematical and Statistical Psychology, 71, 229-261. DOI: 10.1111/bmsp.12110",
    "Hoijtink, H., Mulder, J., van Lissa, C., and Gu, X. (2018). A Tutorial on testing hypotheses using the Bayes factor. Psychological Methods, 24, 539-556. DOI: 10.1037/met0000201.",
    "Hoijtink, H., Gu, X., and Mulder, J. (2018). Bayesian evaluation of informative hypotheses for multiple populations. British Journal of Mathematical and Statistical Psychology, 72, 219-243. DOI: 10.1111/bmsp.12145"
  )
  return(citations)
}

.bainGetContainer <- function(jaspResults, deps) {
  if (is.null(jaspResults[["bainContainer"]])) {
    jaspResults[["bainContainer"]] <- createJaspContainer()
    jaspResults[["bainContainer"]]$dependOn(options = deps)
    jaspResults[["bainContainer"]]$position = 1
  }
  invisible(jaspResults[["bainContainer"]])
}

### DO CURRENT OPTIONS ALLOW FOR ANALYSIS? ###
.bainOptionsReady <- function(options, type, dataset = NULL){
	if(type == "independentTTest"){
		ready <- length(options[["variables"]][options[["variables"]] != ""] > 0) && options[["groupingVariable"]] != ""
	} else if(type == "pairedTTest"){
		ready <- !is.null(unlist(options[["pairs"]]))
	} else if(type == "onesampleTTest"){
		ready <- length(options[["variables"]][options[["variables"]] != ""] > 0)
	} else if(type == "anova"){
		ready <- options[["fixedFactors"]] != "" && options[["dependent"]] != ""
	} else if(type == "ancova"){
		ready <- options[["dependent"]] != "" && options[["fixedFactors"]] != ""  && !is.null(unlist(options[["covariates"]]))
	} else if(type == "regression"){
		ready <- (options[["dependent"]] != "" && unlist(options[["covariates"]]) != "" && !is.null(unlist(options[["covariates"]])))
	} else if(type == "sem"){
		ready <- .bainSemIsReady(options, dataset)
	}
	return(ready)
}

.bainBayesFactorMatrix <- function(dataset, options, bainContainer, ready, type, position) {
  
  if (!is.null(bainContainer[["bayesFactorMatrix"]]) || !options[["bayesFactorMatrix"]]) return()
  
  bayesFactorMatrix <- createJaspTable(gettext("Bayes Factor Matrix"))
  bayesFactorMatrix$position <- position
  
  if (type == "regression")
    bayesFactorMatrix$dependOn(options = c("bayesFactorMatrix", "standardized", "seed"))
  
  if (type == "ancova" || type == "anova" || type == "sem")
    bayesFactorMatrix$dependOn(options = c("bayesFactorMatrix", "seed"))
  
  bayesFactorMatrix$addColumnInfo(name = "hypothesis",  title = "",             type = "string")
  bayesFactorMatrix$addColumnInfo(name = "H1",          title = gettext("H1"),  type = "number")
  
  bainContainer[["bayesFactorMatrix"]] <- bayesFactorMatrix
  
  if (!ready || bainContainer$getError()) {
    row <- data.frame(hypothesis = gettext("H1"), H1 = ".")
    bayesFactorMatrix$addRows(row)
    return()
  }
  
  bainResult <- bainContainer[["bainResult"]]$object
  BFmatrix <- bainResult[["BFmatrix"]]
  
  if (nrow(BFmatrix) > 1) {
    for (i in 2:nrow(BFmatrix))
      bayesFactorMatrix$addColumnInfo(name = paste0("H", i), title = gettextf("H%i", i), type = "number")
  }
  
  for (i in 1:nrow(BFmatrix)) {
    tmp <- list(hypothesis = gettextf("H%i", i))
    for (j in 1:ncol(BFmatrix)) {
      tmp[[paste0("H", j)]] <- BFmatrix[i,j]
    }
    row <- tmp
    bayesFactorMatrix$addRows(row)
  }
}

.bainLinearRegressionBayesFactorPlots <- function(dataset, options, bainContainer, ready, position) {
  
  if (!is.null(bainContainer[["bayesFactorPlot"]]) || !options[["bayesFactorPlot"]]) return()
  
  if(options[["model"]] == ""){
    height <- 300
    width <- 400
  } else {
    height <- 400
    width <- 600
  }
  
  bayesFactorPlot <- createJaspPlot(plot = NULL, title = gettext("Posterior Probabilities"), height = height, width = width)
  bayesFactorPlot$dependOn(options = c("bayesFactorPlot"))
  bayesFactorPlot$position <- position
  
  bainContainer[["bayesFactorPlot"]] <- bayesFactorPlot
  
  if (!ready || bainContainer$getError())
    return()
  
  bainResult <- bainContainer[["bainResult"]]$object
  bayesFactorPlot$plotObject <- .suppressGrDevice(.plot_bain_regression_cran(bainResult))
}