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

BainRegressionLinearBayesian <- function(jaspResults, dataset, options, ...) {
  
  # Check if current options allow for analysis
  ready <- .bainOptionsReady(options, type = "regression")
  
  # Read the data set
  dataList <- .bainReadDataset(options, type = "regression", dataset)
  
  # Check if current data allow for analysis
  .bainDataReady(dataset, options, type = "regression")
  
  # Create a container for the results
  bainContainer <- .bainGetContainer(jaspResults, deps = c("dependent", "covariates", "model", "standardized", "seed"))
  
  # Create a legend containing the order constrained hypotheses
  .bainLegend(dataList[["dataset"]], options, type = "regression", jaspResults, position = 0)
  
  ### RESULTS ###
  .bainLinearRegressionResultsTable(dataList[["dataset"]], options, bainContainer, dataList[["missing"]], ready, position = 1)
  
  ### BAYES FACTOR MATRIX ###
  .bainBayesFactorMatrix(dataList[["dataset"]], options, bainContainer, ready, type = "regression", position = 2)
  
  ### COEFFICIENTS ###
  .bainLinearRegressionCoefficientsTable(dataList[["dataset"]], options, bainContainer, ready, position = 3)
  
  ### POSTERIOR PROBABILITIES PLOT ###
  .bainLinearRegressionBayesFactorPlots(dataList[["dataset"]], options, bainContainer, ready, position = 4)
}

.bainLinearRegressionResultsTable <- function(dataset, options, bainContainer, missingValuesIndicator, ready, position) {
  
  if (!is.null(bainContainer[["bainTable"]])) return() #The options for this table didn't change so we don't need to rebuild it
  
  variables <- c(options[["dependent"]], unlist(options[["covariates"]]))
  
  bainTable <- createJaspTable(gettext("Bain Linear Regression"))
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
  
  if (any(variables %in% missingValuesIndicator)) {
    i <- which(variables %in% missingValuesIndicator)
    if (length(i) > 1) {
      bainTable$addFootnote(message= gettextf("The variables %s contain missing values, the rows containing these values are removed in the analysis.", paste(variables[i], collapse = ", ")), symbol=gettext("<b>Warning.</b>"))
    } else if (length(i) == 1) {
      bainTable$addFootnote(message= gettextf("The variable %s contains missing values, the rows containing these values are removed in the analysis.", variables[i]), symbol=gettext("<b>Warning.</b>"))
    }
  }
  
  if (options$model == "") {
    rest.string <- NULL
  } else {
    rest.string <- .v(.bainCleanModelInput(options[["model"]]))
  }
  
  p <- try({
    bainResult <- bain:::bain_regression_cran(X = dataset, dep = .v(options[["dependent"]]), pred = paste(.v(options[["covariates"]]), collapse = " "), hyp = rest.string, std = options[["standardized"]], seed = options[["seed"]])
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

.bainLinearRegressionCoefficientsTable <- function(dataset, options, bainContainer, ready, position) {
  
  if (!is.null(bainContainer[["coefficientsTable"]]) || !options[["coefficients"]]) return()
  
  coefficientsTable <- createJaspTable(gettext("Coefficients"))
  coefficientsTable$dependOn(options = c("coefficients", "CredibleInterval"))
  coefficientsTable$position <- position
  
  overTitle <- gettextf("%i%% Credible Interval", round(options[["CredibleInterval"]] * 100))
  
  coefficientsTable$addColumnInfo(name="v",       title=gettext("Covariate"),   type="string")
  coefficientsTable$addColumnInfo(name="N",     	title=gettext("N"), 	 	 	    type="integer")
  coefficientsTable$addColumnInfo(name="mean",    title=gettext("Coefficient"), type="number")
  coefficientsTable$addColumnInfo(name="SE",      title=gettext("SE"),          type="number")
  coefficientsTable$addColumnInfo(name="CiLower", title=gettext("Lower"),     	type="number", overtitle = overTitle)
  coefficientsTable$addColumnInfo(name="CiUpper", title=gettext("Upper"),     	type="number", overtitle = overTitle)
  
  if(options[["standardized"]])
    coefficientsTable$addFootnote(message = gettext("The displayed coefficients are standardized."))
  
  bainContainer[["coefficientsTable"]] <- coefficientsTable
  
  if (!ready || bainContainer$getError())
    return()
  
  bainResult <- bainContainer[["bainResult"]]$object
  bainSummary <- summary(bainResult, ci = options[["CredibleInterval"]])
  
  # Extract names, mean and n from bain result
  groups <- .unv(bainSummary[["Parameter"]])
  N <- bainSummary[["n"]]
  mu <- bainSummary[["Estimate"]]
  CiLower <- bainSummary[["lb"]]
  CiUpper <- bainSummary[["ub"]]
  
  # Standard error according to bain package
  se <- sqrt(diag(bainResult$posterior))
  
  row <- data.frame(v = groups, N = N, mean = mu, SE = se, CiLower = CiLower, CiUpper = CiUpper)
  coefficientsTable$addRows(row)
}
