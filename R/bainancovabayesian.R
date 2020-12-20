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
  
  # Create a table containing the main analysis results
  .bainResultsTable(dataList[["dataset"]], options, bainContainer, dataList[["missing"]], ready, type = "ancova", position = 1)
  
  # Create the Bayes factor matrix
  .bainBfMatrix(dataList[["dataset"]], options, bainContainer, ready, type = "ancova", position = 2)
  
  # Create the descriptive statistics (coefficients) table
  .bainDescriptivesTable(dataList[["dataset"]], options, bainContainer, ready, type = "ancova", position = 3)
  
  ### POSTERIOR PROBABILITIES PLOT ###
  .bainAnovaBayesFactorPlots(dataList[["dataset"]], options, bainContainer, ready, position = 4)
  
  ### DESCRIPTIVES PLOT ###
  .bainAnovaDescriptivesPlot(dataList[["dataset"]], options, bainContainer, ready, type = "ancova", position = 5)
}
