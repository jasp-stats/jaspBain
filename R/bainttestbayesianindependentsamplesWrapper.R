#
# Copyright (C) 2013-2024 University of Amsterdam
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

# This is a generated file. Don't change it

BainTTestBayesianIndependentSamples <- function(
          data = NULL,
          version = "0.19.2",
          bayesFactorPlot = FALSE,
          bayesFactorType = "BF01",
          credibleInterval = 0.95,
          descriptives = FALSE,
          descriptivesPlot = FALSE,
          fraction = 1,
          groupingVariable = list(types = list(), value = ""),
          hypothesis = "equalNotEqual",
          plotHeight = 320,
          plotWidth = 480,
          seed = 100,
          variables = list(types = list(), value = NULL)) {

   defaultArgCalls <- formals(jaspBain::BainTTestBayesianIndependentSamples)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL

   optionsWithFormula <- c("groupingVariable", "variables")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspBain::BainTTestBayesianIndependentSamples", data, options, version))
}
