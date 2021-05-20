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
  bainContainer <- .bainGetContainer(jaspResults, deps = c("syntax", "model", "seed", "fraction", "standardized", "fixedFactors", "factorStandardisation"))
  
  # Create a legend containing the order constrained hypotheses
  .bainLegend(dataList[["dataset"]], options, type, jaspResults, position = 0)
  
  # Create a table containing the main analysis results
  .bainTestResultsTable(dataList[["dataset"]], options, bainContainer, dataList[["missing"]], ready, type, position = 1)
  
  # Create the Bayes factor matrix
  .bainBfMatrix(dataList[["dataset"]], options, bainContainer, ready, type, position = 2)
  
  # Create the descriptive statistics (coefficients) table
  .bainDescriptivesTable(dataList[["dataset"]], options, bainContainer, ready, type, position = 3)
  
  # Create the posterior probability plots
  .bainPosteriorProbabilityPlot(dataList[["dataset"]], options, bainContainer, ready, type, position = 4)
  
  # Create the path diagram plot
  .bainSemPathDiagram(dataList[["dataset"]], options, bainContainer, ready, jaspResults, position = 5)
}

.bainSemGetUsedVars <- function(syntax, availablevars) {
  vv <- availablevars
  # This regex will isolate all manifest variables in the SEM model
  # Model:
  # A =~ Ab + Al + Af + An + Ar + Ac 
  # B =~ Bb + Bl + Bf + Bn + Br + Bc 
  # Returns:
  # Ab, Al, Af, An, Ar, Ac, Bb, Bl, Bf, Bn, Br, Bc
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
  withSingleQuotes <- paste("\\b'", usedvars, "'\\b", sep="")
  withDoubleQuotes <- paste('\\b"', usedvars, '"\\b', sep="")
  newNames <- usedvars
  for (i in 1:length(usedvars)) {
    syntax <- gsub(withDoubleQuotes[i], newNames[i], syntax)
  }
  for (i in 1:length(usedvars)) {
    syntax <- gsub(withSingleQuotes[i], newNames[i], syntax)
  }
  for (i in 1:length(usedvars)) {
    syntax <- gsub(paste0("\\b", usedvars[i], "\\b"), newNames[i], syntax)
  }
  return(syntax)
}

.bainSemPathDiagram <- function(dataset, options, bainContainer, ready, jaspResults, position) {
  
  if (!is.null(bainContainer[["pathDiagram"]]) || !options[["pathDiagram"]])
    return()
  
  if (options[["fixedFactors"]] == "") {
    plot <- createJaspPlot(title = gettext("Path Diagram"), width = 600, height = 400)
    plot$dependOn(options = c("pathDiagram", "seed", "pathDiagramEstimates", "pathDiagramLegend"))
    bainContainer[["pathDiagram"]] <- plot
  } else {
    container <- createJaspContainer(title = gettext("Path Diagram"))
    bainContainer[["pathDiagram"]] <- container
    container$dependOn(options = c("pathDiagram", "seed", "pathDiagramEstimates", "pathDiagramLegend"))
    groups <- levels(as.factor(dataset[, encodeColNames(options[["fixedFactors"]])]))
    for(i in 1:length(groups)) {
      plot <- createJaspPlot(title = gettextf("Group: %1$s", groups[i]), width = 600, height = 400)
      container[[paste0("pathDiagram", groups[i])]] <- plot
    }
  }
  
  if (!ready || is.null(bainContainer[["lavaanResult"]]))
    return()
  
  labels <- ifelse(options[["pathDiagramEstimates"]], "par", "name")
  labels <- ifelse(options[["standardized"]], "stand", labels)
  
  fit <- bainContainer[["lavaanResult"]]$object
  po <- .bainlavToPlotObj(fit)
  error <- try({
    pp <- .suppressGrDevice(semPlot::semPaths(
      object         = po,
      layout         = "tree2",
      intercepts     = FALSE,
      reorder        = FALSE,
      whatLabels     = labels,
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
      panelGroups    = TRUE,
      ask            = FALSE
    ))
  })
  
  if (isTryError(error)) {
    plot$setError(gettextf("An error occurred while creating this plot:<br>%1$s.", jaspBase::.extractErrorMessage(error)))
  } else {
    if (options[["fixedFactors"]] == "") {
      plot$plotObject <- pp
    } else {
      for(i in 1:length(groups)) {
        container[[paste0("pathDiagram", groups[i])]]$plotObject <- pp[[i]]
      }
    }
  }
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