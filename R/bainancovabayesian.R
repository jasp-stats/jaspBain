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

BainAncovaBayesian <- function(jaspResults, dataset, options, ...) {
  
  # Check if current options allow for analysis
  ready <- .bainOptionsReady(options, type = "ancova")
  
  # Read the data set 
  dataList <- .bainReadDataset(options, type = "ancova", dataset)
  
  # Check if current data allow for analysis
  .bainDataReady(dataList[["dataset"]], options, type = "ancova")
  
  # Create a container for the results
  bainContainer <- .bainGetContainer(jaspResults, deps = c("dependent", "fixedFactors", "covariates", "model"))
  
  # Create a legend containing the order constrained hypotheses
  .bainLegend(dataList[["dataset"]], options, type = "ancova", jaspResults, position = 0)
  
  ### RESULTS ###
  .bainAncovaResultsTable(dataList[["dataset"]], options, bainContainer, dataList[["missing"]], ready, position = 1)
  
  # Create the Bayes factor matrix
  .bainBfMatrix(dataList[["dataset"]], options, bainContainer, ready, type = "ancova", position = 2)
  
  ### COEFFICIENTS ###
  .bainAnovaDescriptivesTable(dataList[["dataset"]], options, bainContainer, ready, type = "ancova", position = 3)
  
  ### POSTERIOR PROBABILITIES PLOT ###
  .bainAnovaBayesFactorPlots(dataList[["dataset"]], options, bainContainer, ready, position = 4)
  
  ### DESCRIPTIVES PLOT ###
  .bainAnovaDescriptivesPlot(dataList[["dataset"]], options, bainContainer, ready, type = "ancova", position = 5)
}

.bainAncovaResultsTable <- function(dataset, options, bainContainer, missingValuesIndicator, ready, position) {
  
  if (!is.null(bainContainer[["bainTable"]])) return() #The options for this table didn't change so we don't need to rebuild it
  
  variables <- c(options[["dependent"]], options[["fixedFactors"]], unlist(options[["covariates"]]))
  bainTable <- createJaspTable("Bain ANCOVA")
  
  bainTable$addColumnInfo(name="hypotheses", type="string", title="")
  bainTable$addColumnInfo(name="BF", 				 type="number", title= gettext("BF.c"))
  bainTable$addColumnInfo(name="PMP1", 			 type="number", title= gettext("PMP a"))
  bainTable$addColumnInfo(name="PMP2", 			 type="number", title= gettext("PMP b"))
  bainTable$position <- position
  
  message <-  gettext("BF.c denotes the Bayes factor of the hypothesis in the row versus its complement.\
Posterior model probabilities (a: excluding the unconstrained hypothesis, b: including the unconstrained hypothesis) are based on equal prior model probabilities.")
  bainTable$addFootnote(message=message)
  
  bainTable$addCitation(.bainGetCitations())
  
  bainContainer[["bainTable"]] <- bainTable
  
  if (!ready)
    return()
  
  if (any(variables %in% missingValuesIndicator)) {
    i <- which(variables %in% missingValuesIndicator)
    if (length(i) > 1) {
      bainTable$addFootnote(message= gettextf("The variables %s contain missing values, the rows containing these values are removed in the analysis.", paste(variables[i], collapse = ", ")), symbol=gettext("<b>Warning.</b>"))
    } else if (length(i) == 1) {
      bainTable$addFootnote(message= gettextf("The variable %s contains missing values, the rows containing these values are removed in the analysis.", variables[i]), symbol=gettext("<b>Warning.</b>"))
    }
  }
  
  groupCol <- dataset[ , .v(options[["fixedFactors"]])]
  varLevels <- levels(groupCol)
  
  if (length(varLevels) > 15) {
    bainTable$setError(gettext("The fixed factor has too many levels for a Bain analysis."))
    return()
  }
  
  if (options[["model"]] == "") {
    rest.string <- NULL
  } else {
    rest.string <- encodeColNames(.bainCleanModelInput(options[["model"]]))
  }

  p <- try({
    bainResult <- bain:::bain_ancova_cran(X = dataset, dep = .v(options[["dependent"]]), cov = paste(.v(options[["covariates"]]), collapse = " "), group = .v(options[["fixedFactors"]]), hyp = rest.string, seed = options[["seed"]])
    bainContainer[["bainResult"]] <- createJaspState(bainResult)
  })
  
  if (isTryError(p)) {
    bainContainer$setError(gettextf("An error occurred in the analysis:<br>%s<br><br>Please double check your variables and model constraints.", .unv(.extractErrorMessage(p))))
    return()
  }
  
  for (i in 1:(length(bainResult$fit$BF)-1)) {
    row <- list(hypotheses = gettextf("H%i",i), BF = bainResult$fit$BF[i], PMP1 = bainResult$fit$PMPa[i], PMP2 = bainResult$fit$PMPb[i])
    bainTable$addRows(row)
  }
  row <- list(hypotheses = gettext("Hu"), BF = "", PMP1 = "", PMP2 = bainResult$fit$PMPb[length(bainResult$fit$BF)])
  bainTable$addRows(row) 
}
