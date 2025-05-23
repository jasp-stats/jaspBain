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

BainTTestBayesianPairedSamplesInternal <- function(jaspResults, dataset, options, ...) {

  # What type of Bain analysis is being conducted?
  type <- "pairedTTest"

  # Check if current options allow for analysis
  ready <- .bainOptionsReady(options, type)

  # Read the data set
  dataList <- .bainReadDataset(options, type, dataset)

  # Check if current data allow for analysis
  .bainDataReady(dataList[["dataset"]], options, type)

  # Create a container for the results
  bainContainer <- .bainGetContainer(jaspResults, deps = c("seed", "fraction"))

  # Create a table containing the main analysis results
  .bainTestResultsTable(dataList[["dataset"]], options, bainContainer, dataList[["missing"]], ready, type, position = 1)

  # Create the descriptive statistics table
  .bainDescriptivesTable(dataList[["dataset"]], options, bainContainer, ready, type, position = 2)

  # Create the posterior probability plots
  .bainPosteriorProbabilityPlot(dataList[["dataset"]], options, bainContainer, ready, type, position = 3)

  # Create the descriptive plot(s)
  .bainDescriptivePlots(dataList[["dataset"]], options, bainContainer, ready, type, position = 4)
}
