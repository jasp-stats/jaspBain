#
# Copyright (C) 2013-2025 University of Amsterdam
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

# This is a generated file. Don't change it!

#' Bain ANOVA
#'
BainAnovaBayesian <- function(
          data = NULL,
          version = "0.95",
          bayesFactorMatrix = FALSE,
          bayesFactorPlot = FALSE,
          credibleInterval = 0.95,
          dependent = list(types = list(), value = ""),
          descriptives = FALSE,
          descriptivesPlot = FALSE,
          fixedFactors = list(types = list(), value = ""),
          fraction = 1,
          model = "",
          plotHeight = 320,
          plotWidth = 480,
          seed = 33009,
          standardized = FALSE) {

   defaultArgCalls <- formals(jaspBain::BainAnovaBayesian)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("dependent", "fixedFactors", "model")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspBain", "BainAnovaBayesian", "BainAnovaBayesian.qml", options, version, TRUE))
}