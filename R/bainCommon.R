##################
### HELPERS ######
##################

# Clean the input for the order constraints
.bainCleanModelInput <- function(input) {
  return(gsub("\n+", ";", input))
}

# Add the Bain citations
.bainGetCitations <- function() {
  citations <- c(
    "Gu, X., Mulder, J., and Hoijtink, H. (2017). Approximate adjusted fractional Bayes factors: A general method for testing informative hypotheses. British Journal of Mathematical and Statistical Psychology, 71, 229-261. DOI: 10.1111/bmsp.12110",
    "Hoijtink, H., Mulder, J., van Lissa, C., and Gu, X. (2018). A Tutorial on testing hypotheses using the Bayes factor. Psychological Methods, 24, 539-556. DOI: 10.1037/met0000201.",
    "Hoijtink, H., Gu, X., and Mulder, J. (2018). Bayesian evaluation of informative hypotheses for multiple populations. British Journal of Mathematical and Statistical Psychology, 72, 219-243. DOI: 10.1111/bmsp.12145"
  )
  return(citations)
}

# Create a container for the results
.bainGetContainer <- function(jaspResults, deps) {
  if (is.null(jaspResults[["bainContainer"]])) {
    jaspResults[["bainContainer"]] <- createJaspContainer()
    jaspResults[["bainContainer"]]$dependOn(options = deps)
    jaspResults[["bainContainer"]]$position = 1
  }
  invisible(jaspResults[["bainContainer"]])
}

# Read the data set
.bainReadDataset <- function(options, type, dataset){
  numerics <- switch(type,
                     "onesampleTTest" = options[["variables"]],
                     "pairedTTest" = unique(unlist(options[["pairs"]])),
                     "independentTTest" = unlist(options[["variables"]]),
                     "anova" = options[["dependent"]],
                     "ancova" = c(options[["dependent"]], unlist(options[["covariates"]])),
                     "regression" = c(options[["dependent"]], unlist(options[["covariates"]])),
                     "sem" = NULL)
  numerics <- numerics[numerics != ""]
  factors <- switch(type,
                    "onesampleTTest" = NULL,
                    "pairedTTest" = NULL,
                    "independentTTest" = options[["groupingVariable"]],
                    "anova" = options[["fixedFactors"]],
                    "ancova" = options[["fixedFactors"]],
                    "regression" = NULL,
                    "sem" = NULL)
  factors <- factors[factors != ""]
  vars <- c(numerics, factors)
  if(type != "sem"){
    if (is.null(dataset)) {
      trydata	<- .readDataSetToEnd(columns.as.numeric = numerics, columns.as.factor = factors)
      missing	<- .unv(names(which(apply(trydata, 2, function(x) { any(is.na(x))} ))))
      dataset	<- .readDataSetToEnd(columns.as.numeric = numerics, columns.as.factor = factors, exclude.na.listwise = vars)
      
      if((type == "anova" || type == "ancova") && options[["fixedFactors"]] != ""){
        if(any(grepl(pattern = " ", x = levels(dataset[, .v(options[["fixedFactors"]])])))){
          jaspBase:::.quitAnalysis(gettext("Bain does not accept factor levels that contain spaces. Please remove the spaces from your factor levels to continue."))
        }
      }
    } else {
      dataset 	<- .vdf(dataset, columns.as.numeric = numerics, columns.as.factor = factors)
    } 
  } else {
    trydata 	<- .readDataSetToEnd(all.columns = TRUE)
    missing		<- .unv(names(which(apply(trydata, 2, function(x) { any(is.na(x))} ))))
    dataset		<- .readDataSetToEnd(all.columns = TRUE, exclude.na.listwise = .bainSemGetUsedVars(options[["syntax"]], colnames(trydata)))
  }
  readList <- list()
  readList[["dataset"]] <- dataset
  readList[["missing"]] <- missing
  return(readList)
}

##################
### Checks #######
##################

# Check if current options allow for analysis
.bainOptionsReady <- function(options, type, dataset = NULL){
  ready <- switch(type,
                  "independentTTest" = length(options[["variables"]][options[["variables"]] != ""] > 0) && options[["groupingVariable"]] != "",
                  "pairedTTest" = !is.null(unlist(options[["pairs"]])),
                  "onesampleTTest" = length(options[["variables"]][options[["variables"]] != ""] > 0),
                  "anova" = options[["fixedFactors"]] != "" && options[["dependent"]] != "",
                  "ancova" = options[["dependent"]] != "" && options[["fixedFactors"]] != ""  && !is.null(unlist(options[["covariates"]])),
                  "regression" = (options[["dependent"]] != "" && unlist(options[["covariates"]]) != "" && !is.null(unlist(options[["covariates"]]))),
                  "sem" = length(.bainSemGetUsedVars(options[["syntax"]], colnames(dataset))) > 1)
  return(ready)
}

# Check if current data allow for analysis
.bainDataReady <- function(dataset, options, type){
  if(type == "independentTTest"){
    factors <- options[["groupingVariable"]]
    factors <- factors[factors != ""]
    if(length(factors) > 0)
      .hasErrors(dataset, type = "factorLevels",
                 factorLevels.target = factors, factorLevels.amount = "!= 2",
                 exitAnalysisIfErrors = TRUE)
  }  
  numerics <- switch(type,
                     "onesampleTTest" = options[["variables"]],
                     "pairedTTest" = unique(unlist(options[["pairs"]])),
                     "independentTTest" = unlist(options[["variables"]]),
                     "anova" = options[["dependent"]],
                     "ancova" = c(options[["dependent"]], unlist(options[["covariates"]])),
                     "regression" = c(options[["dependent"]], unlist(options[["covariates"]])),
                     "sem" = NULL)
  numerics <- numerics[numerics != ""]
  if(length(numerics) > 0)
    .hasErrors(dataset, type = c("infinity", "variance", "observations"),
               all.target = numerics, observations.amount = "< 3",
               exitAnalysisIfErrors = TRUE)
}

##################
### TABLES #######
##################

# Create a legend containing the order constrained hypotheses
.bainLegend <- function(dataset, options, type, jaspResults, position) {
  if (!is.null(jaspResults[["legendTable"]])) 
    return()
  legendTable <- createJaspTable("Hypothesis Legend")
  deps <- switch(type, 
                 "regression" = c("model", "covariates"),
                 "anova" = c("model", "fixedFactors"),
                 "ancova" = c("model", "fixedFactors"),
                 "sem" = c("model", "syntax"))
  legendTable$dependOn(options = deps)
  legendTable$position <- position
  legendTable$addColumnInfo(name = "number",     type = "string", title = "")
  legendTable$addColumnInfo(name = "hypothesis", type = "string", title = gettext("Hypothesis"))
  jaspResults[["legendTable"]] <- legendTable
  if (options[["model"]] != "") {
    rest.string <- .bainCleanModelInput(options[["model"]])
    hyp.vector <- unlist(strsplit(rest.string, "[;]")) 
    for (i in 1:length(hyp.vector)) {
      row <- list(number = gettextf("H%i", i), hypothesis = hyp.vector[i])
      legendTable$addRows(row)
    }
  } else {
    if(type == "regression"){
      variables <- options[["covariates"]]
      if (length(variables) == 0) {
        row <- list(number = gettext("H1"), hypothesis = "")
      } else if (length(variables) == 1) {
        row <- list(number = gettext("H1"), hypothesis = paste(variables, "= 0"))
      } else {
        row <- list(number = gettext("H1"), hypothesis = paste0(paste0(variables, " = 0"), collapse = " & "))
      }
    } else if(type == "anova" || type == "ancova"){
      if (options[["fixedFactors"]] != "") {
        string <- paste(paste(options[["fixedFactors"]], levels(dataset[, .v(options[["fixedFactors"]])]), sep = ""), collapse = " = ")
        row <- list(number = gettext("H1"), hypothesis = string)
      } else {
		row <- list(number = gettext("H1"), hypothesis = "")
	  }
    } else if(type == "sem"){
      variables <- .bainSemGetUsedVars(options[["syntax"]], colnames(dataset))
      if (length(variables) == 0) {
        row <- list(number = gettext("H1"), hypothesis = "")
      } else if (length(variables) == 1) {
        row <- list(number = gettext("H1"), hypothesis = paste(variables, "= 0")) # Needs to be adjusted
      } else {
        row <- list(number = gettext("H1"), hypothesis = paste0(paste0(variables, " = 0"), collapse = " & ")) # Needs to be adjusted
      }
    }
	legendTable$addRows(row)
  }
}

# Create the Bayes factor matrix
.bainBfMatrix <- function(dataset, options, bainContainer, ready, type, position) {
  if (!is.null(bainContainer[["bayesFactorMatrix"]]) || !options[["bayesFactorMatrix"]]) 
    return() 
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

##################
### PLOTS ########
##################

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

.bainTTestFactorPlots <- function(dataset, options, bainContainer, ready, type, position) {
  
  if (!is.null(bainContainer[["bayesFactorPlots"]]) || !options[["bayesFactorPlot"]]) return()
  
  bayesFactorPlots <- createJaspContainer(gettext("Posterior Probabilities"))
  bayesFactorPlots$dependOn(options = c("variables", "bayesFactorPlot", "hypothesis", "pairs"))
  bayesFactorPlots$position <- position
  
  bainContainer[["bayesFactorPlots"]] <- bayesFactorPlots
  
  if (!ready)
    return()
  
  analysisType <- base::switch(options[["hypothesis"]],
                               "equalNotEqual"		= 1,
                               "equalBigger"		  = 2,
                               "equalSmaller"		= 3,
                               "biggerSmaller"		= 4,
                               "equalBiggerSmaller"= 5)
  
  if(type == "oneSample" || type == "independentSamples"){
    
    for(variable in options[["variables"]]){
      
      if (is.null(bayesFactorPlots[[variable]])){
        
        bainAnalysis <- .bainOneSampleState(variable, options, dataset, bainContainer)
        
        plot <- createJaspPlot(plot = NULL, title = variable, height = 300, width = 400)
        plot$dependOn(optionContainsValue=list("variables" = variable))
        
        if(isTryError(bainAnalysis)){
          plot$setError(gettext("Plotting not possible: the results for this variable were not computed."))
        } else {
          p <- try({
            plot$plotObject <- .plot_bain_ttest_cran(bainAnalysis, type = analysisType)
          })
          if(isTryError(p)){
            plot$setError(gettextf("Plotting not possible: %s", .extractErrorMessage(p)))
          }
        }    
        bayesFactorPlots[[variable]] <- plot
      }
    }
    
  } else if (type == "pairedSamples"){
    
    for(pair in options[["pairs"]]){
      
      currentPair <- paste(pair, collapse=" - ")
      
      if (is.null(bayesFactorPlots[[currentPair]]) && pair[[2]] != "" && pair[[1]] != pair[[2]]){
        
        bainAnalysis <- .bainPairedSampleState(pair, options, dataset, bainContainer)
        
        plot <- createJaspPlot(plot = NULL, title = currentPair, height = 300, width = 400)
        plot$dependOn(optionContainsValue=list("pairs" = pair))
        
        if(isTryError(bainAnalysis)){
          plot$setError(gettext("Plotting not possible: the results for this variable were not computed."))
        } else {
          p <- try({
            plot$plotObject <- .plot_bain_ttest_cran(bainAnalysis, type = analysisType)
          })
          if(isTryError(p)){
            plot$setError(gettextf("Plotting not possible: %s", .extractErrorMessage(p)))
          }
        }    
        bayesFactorPlots[[currentPair]] <- plot
      }
    }
  }
}

.plot_bain_ttest_cran <- function(x, type){
  
  if(type == 1 || type == 2 || type == 3){
    labs <- c(gettext("H0"), gettext("H1"))
  }
  if(type == 4){
    labs <- c(gettext("H1"), gettext("H2"))
  }
  if(type == 5){
    labs <- c(gettext("H0"), gettext("H1"), gettext("H2"))
  }
  labels <- rev(labs)
  if(type == 1){
    values <- x$fit$PMPb
  } else {
    values <- na.omit(x$fit$PMPa)
  }
  
  ggdata <- data.frame(lab = labs, PMP = values)
  
  p <- ggplot2::ggplot(data = ggdata, mapping = ggplot2::aes(x = "", y = PMP, fill = lab)) +
    ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
    ggplot2::geom_col() +
    ggplot2::coord_polar(theta = "y", direction = -1) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "none") +
    ggplot2::scale_y_continuous(breaks = cumsum(rev(values)) - rev(values)/2, labels = labels) +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   axis.text=ggplot2::element_text(size=17, color = "black"),
                   plot.title = ggplot2::element_text(size=18, hjust = .5),
                   axis.ticks.y = ggplot2::element_blank()) +
    ggplot2::scale_fill_brewer(palette="Set1")
  
  return(p)
}

.plot_bain_regression_cran <- function(x)
{
  PMPa <- na.omit(x$fit$PMPa)
  PMPb <- x$fit$PMPb
  numH <- length(PMPa)
  P_lables <- paste(gettext("H"), 1:numH, sep = "")
  ggdata1 <- data.frame(lab = P_lables, PMP = PMPa)
  ggdata2 <- data.frame(lab = c(P_lables, gettext("Hu")), PMP = PMPb)
  
  if (numH == 1) {
    
    p <- ggplot2::ggplot(data = ggdata2, mapping = ggplot2::aes(x = "", y = PMP, fill = lab)) +
      ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
      ggplot2::geom_col() + 
      ggplot2::coord_polar(theta = "y", direction = -1) +
      ggplot2::labs(x = "", y = "", title = "") +
      ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "none") +
      ggplot2::scale_y_continuous(breaks = cumsum(rev(PMPb)) - rev(PMPb)/2, labels = rev(c(P_lables, "Hu"))) +
      ggplot2::theme(panel.background = ggplot2::element_blank(),
                     axis.text=ggplot2::element_text(size=17, color = "black"),
                     plot.title = ggplot2::element_text(size=18, hjust = .5),
                     axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::scale_fill_brewer(palette="Set1")
    
    return(p)
    
  } else if (numH > 1) {
    
    p1 <- ggplot2::ggplot(data = ggdata1, mapping = ggplot2::aes(x = "", y = PMP, fill = lab)) +
      ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
      ggplot2::geom_col() +
      ggplot2::coord_polar(theta = "y", direction = -1) +
      ggplot2::labs(x = "", y = "", title = gettext("Excluding Hu")) +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),legend.position = "none") +
      ggplot2::scale_y_continuous(breaks = cumsum(rev(PMPa)) - rev(PMPa)/2, labels = rev(P_lables)) +            
      ggplot2::theme(panel.background = ggplot2::element_blank(),
                     axis.text=ggplot2::element_text(size=17, color = "black"),
                     plot.title = ggplot2::element_text(size=18, hjust = .5),
                     axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::scale_fill_brewer(palette="Set1")
    
    p2 <- ggplot2::ggplot(data = ggdata2, mapping = ggplot2::aes(x = "", y = PMP, fill = lab)) +
      ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
      ggplot2::geom_col() + 
      ggplot2::coord_polar(theta = "y", direction = -1) +
      ggplot2::labs(x = "", y = "", title = gettext("Including Hu")) +
      ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "none") +
      ggplot2::scale_y_continuous(breaks = cumsum(rev(PMPb)) - rev(PMPb)/2, labels = rev(c(P_lables, "Hu"))) +
      ggplot2::theme(panel.background = ggplot2::element_blank(),
                     axis.text=ggplot2::element_text(size=17, color = "black"),
                     plot.title = ggplot2::element_text(size=18, hjust = .5),
                     axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::scale_fill_brewer(palette="Set1")
    
    plotMat <- list(p1 = p1, p2 = p2)
    pp <- jaspGraphs::ggMatrixPlot(plotList = plotMat, layout = matrix(c(1, 2), ncol = 2))
    
    return(pp)
  }
}

.plot_bain_ancova_cran <- function(x)
{
  PMPa <- na.omit(x$fit$PMPa)
  PMPb <- x$fit$PMPb
  numH <- length(PMPa)
  P_lables <- paste(gettext("H"), 1:numH, sep = "")
  ggdata1 <- data.frame(lab = P_lables, PMP = PMPa)
  ggdata2 <- data.frame(lab = c(P_lables, gettext("Hu")), PMP = PMPb)
  
  if (numH == 1) {
    
    p <- ggplot2::ggplot(data = ggdata2, mapping = ggplot2::aes(x = "", y = PMP, fill = lab)) +
      ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
      ggplot2::geom_col() +
      ggplot2::coord_polar(theta = "y", direction = -1) +
      ggplot2::labs(x = "", y = "", title = "") + 
      ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "none") + 
      ggplot2::scale_y_continuous(breaks = cumsum(rev(PMPb)) - rev(PMPb)/2, labels = rev(c(P_lables, gettext("Hu")))) +
      ggplot2::theme(panel.background = ggplot2::element_blank(), 
                     axis.text=ggplot2::element_text(size=17, color = "black"),
                     plot.title = ggplot2::element_text(size=18, hjust = .5),
                     axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::scale_fill_brewer(palette="Set1")
    
    return(p)
    
  } else if (numH > 1) {
    
    p1 <- ggplot2::ggplot(data = ggdata1, mapping = ggplot2::aes(x = "", y = PMP, fill = lab)) +
      ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
      ggplot2::geom_col() + 
      ggplot2::coord_polar(theta = "y", direction = -1) +
      ggplot2::labs(x = "", y = "", title = gettext("Excluding Hu"), size = 30) +
      ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "none") +
      ggplot2::scale_y_continuous(breaks = cumsum(rev(PMPa)) - rev(PMPa)/2, labels = rev(P_lables)) +
      ggplot2::theme(panel.background = ggplot2::element_blank(),
                     axis.text=ggplot2::element_text(size=17, color = "black"),
                     plot.title = ggplot2::element_text(size=18, hjust = .5),
                     axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::scale_fill_brewer(palette="Set1") 
    
    p2 <- ggplot2::ggplot(data = ggdata2, mapping = ggplot2::aes(x = "", y = PMP, fill = lab)) +
      ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
      ggplot2::geom_col() + 
      ggplot2::coord_polar(theta = "y", direction = -1) +
      ggplot2::labs(x = "", y = "", title = gettext("Including Hu"), size = 30) +
      ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "none") +
      ggplot2::scale_y_continuous(breaks = cumsum(rev(PMPb)) - rev(PMPb)/2, labels = rev(c(P_lables, gettext("Hu")))) +
      ggplot2::theme(panel.background = ggplot2::element_blank(),
                     axis.text=ggplot2::element_text(size=17, color = "black"),
                     plot.title = ggplot2::element_text(size=18, hjust = .5),
                     axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::scale_fill_brewer(palette="Set1")
    
    plotMat <- list(p1 = p1, p2 = p2)
    pp <- jaspGraphs::ggMatrixPlot(plotList = plotMat, layout = matrix(c(1, 2), ncol = 2))
    
    return(pp)
  }
}