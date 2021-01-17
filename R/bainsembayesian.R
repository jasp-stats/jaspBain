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

  # What type of Bain analysis is being conducted?
  type <- "sem"

  # Read the data set
  dataList <- .bainReadDataset(options, type, dataset)

  # Check if current options allow for analysis
  ready <- .bainOptionsReady(options, type, dataList[["dataset"]])

  # Check if current data allow for analysis
  .bainDataReady(dataList[["dataset"]], options, type)
  
  # Create a container for the results
  bainContainer <- .bainGetContainer(jaspResults, deps = c("syntax", "model", "seed", "fraction"))
  
  # Create a legend containing the order constrained hypotheses
  .bainLegend(dataList[["dataset"]], options, type, jaspResults, position = 0)
  
  # Create a table containing the main analysis results
  .bainResultsTable(dataList[["dataset"]], options, bainContainer, dataList[["missing"]], ready, type, position = 1)
  
  # Create the Bayes factor matrix
  .bainBfMatrix(dataList[["dataset"]], options, bainContainer, ready, type, position = 2)
  
  # Create the descriptive statistics (coefficients) table
  .bainDescriptivesTable(dataList[["dataset"]], options, bainContainer, ready, type, position = 3)
  
  # Create the posterior probability plots
  .bainPosteriorProbabilityPlot(dataList[["dataset"]], options, bainContainer, ready, type, position = 4)
  
  # Create the path diagram plot
  .bainSemPathDiagram(dataList[["dataset"]], options, bainContainer, ready, jaspResults, position = 5)
}

.bainLavaanState <- function(dataset, options, bainContainer, ready, jaspResults){
  if(!is.null(bainContainer[["lavaanResult"]])){
    return(bainContainer[["lavaanResult"]]$object)
  } else if(ready){
    syntax <- .bainSemTranslateModel(options[["syntax"]], dataset)
    error <- try({
      fit <- lavaan::sem(model = syntax, data = dataset)
    })
    if(isTryError(error)){
      bainContainer$setError(gettextf("An error occurred in the call to lavaan: %1$s", jaspBase:::.extractErrorMessage(error)))
      return()
    }
    bainContainer[["lavaanResult"]] <- createJaspState(fit)
    bainContainer[["lavaanResult"]]$dependOn(options = "syntax")
    return(bainContainer[["lavaanResult"]]$object)
  }
}

.bainSemGetUsedVars <- function(syntax, availablevars, decode = FALSE) {
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
  usedvars <- .bainSemGetUsedVars(syntax, colnames(dataset))
  if (length(usedvars) == 0)
    return(syntax)
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
  
  plot <- createJaspPlot(title = gettext("Path Diagram"), width = 600, height = 400)
  plot$dependOn(options = c("pathDiagram", "seed", "pathDiagramEstimates", "pathDiagramLegend"))
  bainContainer[["pathDiagram"]] <- plot
  fit <- .bainLavaanState(dataset, options, bainContainer, ready, jaspResults)
  po <- .bainlavToPlotObj(fit)
  pp <- .suppressGrDevice(semPlot::semPaths(
    object         = po,
    layout         = "tree2",
    intercepts     = FALSE,
    reorder        = FALSE,
    whatLabels     = ifelse(options[["pathDiagramEstimates"]], "par", "name"),
    edge.color     = "black",
    color          = list(lat = "#EAEAEA", man = "#EAEAEA", int = "#FFFFFF"),
    title          = FALSE,
    legend         = options[["pathDiagramLegend"]],
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