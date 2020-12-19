#
# Copyright (C) 2013-2015 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

BainSemBayesian <- function(jaspResults, dataset, options, ...) {
  
  # Read the data set
  dataList <- .bainReadDataset(options, type = "sem", dataset)
  
  # Check if current options allow for analysis
  ready <- .bainOptionsReady(options, type = "sem", dataList[["dataset"]])

  # Check if current data allow for analysis
  .bainDataReady(dataList[["dataset"]], options, type = "sem")
  
  # Create a container for the results
  bainContainer <- .bainGetContainer(jaspResults, deps = c("syntax", "model", "seed"))
  
  # Create a legend containing the order constrained hypotheses
  .bainLegend(dataList[["dataset"]], options, type = "sem", jaspResults, position = 0)
  
  .bainSemResultsTable(dataList[["dataset"]], options, ready, bainContainer, jaspResults, position = 1)
  
  ### BAYES FACTOR MATRIX ###
  .bainBayesFactorMatrix(dataList[["dataset"]], options, bainContainer, ready, type = "sem", position = 2)
  
  ### COEFFICIENTS TABLE ###
  .bainSemCoefficientsTable(dataList[["dataset"]], options, bainContainer, ready, position = 3)
  
  ### POSTERIOR PROBABILITIES PLOT ###
  .bainLinearRegressionBayesFactorPlots(dataList[["dataset"]], options, bainContainer, ready, position = 4)
  
  ### PATH DIAGRAM ###
  .bainSemPathDiagram(dataList[["dataset"]], options, bainContainer, ready, jaspResults, position = 5)
}

.fitLavaanModelBain <- function(dataset, options, ready, bainContainer, jaspResults){
  
  if(!is.null(bainContainer[["lavaanResult"]])){
    
    return(bainContainer[["lavaanResult"]]$object)
    
  } else if(ready){
    
    syntax <- .bainSemTranslateModel(options[["syntax"]], dataset)
    colnames(dataset) <- decodeColNames(colnames(dataset))
    error <- try({
      fit <- lavaan::sem(model = syntax, data = dataset)
    })
    
    if(isTryError(error)){
      bainContainer$setError(gettextf("An error occurred in the call to lavaan: %1$s", JASP:::.extractErrorMessage(error)))
      return()
    }
    
    bainContainer[["lavaanResult"]] <- createJaspState(fit)
    bainContainer[["lavaanResult"]]$dependOn(options = "syntax")
    
    return(bainContainer[["lavaanResult"]]$object)
    
  }
}

.fitBainSemModel <- function(dataset, options, ready, bainContainer, jaspResults, rest.string){
  
  if(!is.null(bainContainer[["bainResult"]])){
    
    return(bainContainer[["bainResult"]]$object)
    
  } else if(ready){
    
    fit <- .fitLavaanModelBain(dataset, options, ready, bainContainer, jaspResults)
    
    error <- try({
      bainResult <- bain::bain(x = fit, hypothesis = rest.string)
    })
    
    if(isTryError(error)){
      bainContainer$setError(gettextf("An error occurred in the call to bain: %1$s", JASP:::.extractErrorMessage(error)))
      return()
    }
    
    bainContainer[["bainResult"]] <- createJaspState(bainResult)
    bainContainer[["bainResult"]]$dependOn(options = "model")
    
    return(bainContainer[["bainResult"]]$object)
    
  }
}

.bainSemResultsTable <- function(dataset, options, ready, bainContainer, jaspResults, position){
  
  if (!is.null(bainContainer[["bainTable"]])) 
    return() 
  
  bainTable <- createJaspTable(gettext("Bain Structural Equation Model"))
  bainContainer[["bainTable"]] <- bainTable
  bainTable$position <- position
  
  bainTable$addColumnInfo(name="hypotheses", type="string", title="")
  bainTable$addColumnInfo(name="BF",         type="number", title=gettext("BF.c"))
  bainTable$addColumnInfo(name="PMP1",       type="number", title=gettext("PMP a"))
  bainTable$addColumnInfo(name="PMP2",       type="number", title=gettext("PMP b"))
  
  message <- gettext("BF.c denotes the Bayes factor of the hypothesis in the row versus its complement.\
Posterior model probabilities (a: excluding the unconstrained hypothesis, b: including the unconstrained hypothesis) are based on equal prior model probabilities.")
  bainTable$addFootnote(message=message)
  
  bainTable$addCitation(.bainGetCitations())
  
  if (!ready)
    return()
  
  if (options$model == "") {
    rest.string <- "ind60___by___x2 = 0" # CHANGE
  } else {
    rest.string <- .v(.bainCleanModelInput(options[["model"]]))
  }
  
  bainResult <- .fitBainSemModel(dataset, options, ready, bainContainer, jaspResults, rest.string)
  
  for (i in 1:(length(bainResult$fit$BF)-1)) {
    row <- list(hypotheses = gettextf("H%i",i), BF = bainResult$fit$BF[i], PMP1 = bainResult$fit$PMPa[i], PMP2 = bainResult$fit$PMPb[i])
    bainTable$addRows(row)
  }
  row <- list(hypotheses = gettext("Hu"), BF = "", PMP1 = "", PMP2 = bainResult$fit$PMPb[length(bainResult$fit$BF)])
  bainTable$addRows(row) 
}

.bainSemCoefficientsTable <- function(dataset, options, bainContainer, ready, position){
  
  if (!is.null(bainContainer[["coefficientsTable"]]) || !options[["coefficients"]]) 
    return() 
  
  coefficientsTable <- createJaspTable(gettext("Coefficients"))
  coefficientsTable$position <- position
  coefficientsTable$dependOn(c("coefficients", "CredibleInterval"))
  
  coefficientsTable$addColumnInfo(name="Parameter", type="string", title = "Parameter")
  coefficientsTable$addColumnInfo(name="n", type="integer", title = "n")
  coefficientsTable$addColumnInfo(name="Estimate", type="number", title = "Estimate")
  coefficientsTable$addColumnInfo(name="lb", type="number", title = "Lower", overtitle = gettextf("%1$s%% Credible interval", round(options[["CredibleInterval"]] * 100, 2)))
  coefficientsTable$addColumnInfo(name="ub", type="number", title = "Upper", overtitle = gettextf("%1$s%% Credible interval", round(options[["CredibleInterval"]] * 100, 2)))
  
  bainContainer[["coefficientsTable"]] <- coefficientsTable
  
  if(!ready)
    return()
  
  bainResult <- .fitBainSemModel(dataset, options, ready, bainContainer, jaspResults, rest.string)
  coefficients <- summary(bainResult, ci = options[["CredibleInterval"]])
  coefficientsTable$setData(coefficients)
  
}

# helper functions
.bainSemReadData <- function(dataset, options) {
  if (!is.null(dataset)) 
    return(dataset)
  dataset <- .readDataSetToEnd(all.columns = TRUE)
  return(dataset)
}

.bainSemIsReady <- function(options, dataset) {
  usedvars <- .bainSemGetUsedVars(options[["syntax"]], colnames(dataset))
  if (length(usedvars) > 1) TRUE else FALSE
}

.bainSemGetUsedVars <- function(syntax, availablevars, decode = TRUE) {
  if(decode){
    vv <- decodeColNames(availablevars)
  } else {
    vv <- availablevars
  }
  findpattern <- paste0("(?<=[\\s\\+\\^\\=\\~\\<\\*\\>\\:\\%\\|\\+]|^)\\Q",
                        vv,
                        "\\E(?=[\\s\\+\\^\\=\\~\\<\\*\\>\\:\\%\\|\\+]|$)")
  return(vv[vapply(findpattern,
                   function(p) stringr::str_detect(syntax, p),
                   FUN.VALUE = TRUE,
                   USE.NAMES = FALSE)])
}

.bainSemTranslateModel <- function(syntax, dataset) {
  #' translate model syntax to jasp column names syntax
  usedvars <- .bainSemGetUsedVars(syntax, colnames(dataset))
  
  if (length(usedvars) == 0) {
    return(syntax)
  }
  
  usedvars <- usedvars[order(nchar(usedvars), decreasing = TRUE)]
  with.s.quotes <- paste("\\b'", usedvars, "'\\b", sep="")
  with.d.quotes <- paste('\\b"', usedvars, '"\\b', sep="")
  
  new.names <- .v(usedvars)
  
  for (i in 1:length(usedvars)) {
    syntax <- gsub(with.d.quotes[i], new.names[i], syntax)
  }
  
  for (i in 1:length(usedvars)) {
    syntax <- gsub(with.s.quotes[i], new.names[i], syntax)
  }
  
  for (i in 1:length(usedvars)) {
    syntax <- gsub(paste0("\\b", usedvars[i], "\\b"), new.names[i], syntax)
  }
  
  return(syntax)
}

.bainSemPathDiagram <- function(dataset, options, bainContainer, ready, jaspResults, position) {
  
  if(!is.null(bainContainer[["pathDiagram"]]) || !options[["pathDiagram"]])
    return()
  
  plot <- createJaspPlot(title = gettext("Path diagram"), width = 600, height = 400)
  plot$dependOn(options = "pathDiagram")
  bainContainer[["pathDiagram"]] <- plot
  
  fit <- .fitLavaanModelBain(dataset, options, ready, bainContainer, jaspResults)
  
  # create a qgraph object using semplot
  po <- .bainlavToPlotObj(fit)
  pp <- .suppressGrDevice(semPlot::semPaths(
    object         = po,
    layout         = "tree2",
    intercepts     = FALSE,
    reorder        = FALSE,
    whatLabels     = "par", # ifelse(options[["pathPlotPar"]], "par", "name"),
    edge.color     = "black",
    color          = list(lat = "#EAEAEA", man = "#EAEAEA", int = "#FFFFFF"),
    title          = FALSE,
    legend         = FALSE, #options[["pathPlotLegend"]],
    legend.mode    = "names",
    legend.cex     = 0.6,
    label.cex      = 1.3,
    edge.label.cex = 0.9,
    nodeNames      = decodeColNames(po@Vars$name),
    nCharNodes     = 3,
    rotation       = 2,
    ask            = FALSE
  ))
  
  plot$plotObject <- pp
}

.bainlavToPlotObj <- function(lavResult) {
  # Create semplot model and decode the names of the manifest variables
  # Sorry, this code is really ugly but all it does is replace names for plot.
  semPlotMod <- semPlot::semPlotModel(list(lavResult), list(mplusStd = "std"))[[1]]
  
  manifests <- semPlotMod@Vars$name[semPlotMod@Vars$manifest]
  semPlotMod@Vars$name[semPlotMod@Vars$manifest] <- decodeColNames(manifests)
  
  lhsAreManifest <- semPlotMod@Pars$lhs %in% manifests
  if (any(lhsAreManifest)) 
    semPlotMod@Pars$lhs[lhsAreManifest] <- decodeColNames(semPlotMod@Pars$lhs[lhsAreManifest])
  
  rhsAreManifest <- semPlotMod@Pars$rhs %in% manifests
  if (any(rhsAreManifest)) 
    semPlotMod@Pars$rhs[rhsAreManifest] <- decodeColNames(semPlotMod@Pars$rhs[rhsAreManifest])
  
  return(semPlotMod)
}