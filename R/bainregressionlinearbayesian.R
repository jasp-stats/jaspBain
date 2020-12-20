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
  
  # Create a table containing the main analysis results
  .bainResultsTable(dataList[["dataset"]], options, bainContainer, dataList[["missing"]], ready, type = "regression", position = 1)
  
  # Create the Bayes factor matrix
  .bainBfMatrix(dataList[["dataset"]], options, bainContainer, ready, type = "regression", position = 2)
  
  ### COEFFICIENTS ###
  .bainLinearRegressionCoefficientsTable(dataList[["dataset"]], options, bainContainer, ready, position = 3)
  
  ### POSTERIOR PROBABILITIES PLOT ###
  .bainLinearRegressionBayesFactorPlots(dataList[["dataset"]], options, bainContainer, ready, position = 4)
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
