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

BainTTestBayesianOneSample <- function(jaspResults, dataset, options, ...) {
  
  # Check if current options allow for analysis
  ready <- .bainOptionsReady(options, type = "onesampleTTest")
  
  # Read the data set 
  dataList <- .bainReadDataset(options, type = "onesampleTTest", dataset)
  
  # Check if current data allow for analysis
  .bainDataReady(dataset, options, type = "onesampleTTest")
  
  # Create a container for the results
  bainContainer <- .bainGetContainer(jaspResults, deps = c("testValue", "seed"))
  
  # Create a table containing the main analysis results
  .bainResultsTable(dataList[["dataset"]], options, bainContainer, dataList[["missing"]], ready, type = "onesampleTTest", position = 1)
  
  # Create the descriptive statistics table
  .bainDescriptivesTable(dataList[["dataset"]], options, bainContainer, ready, type = "onesampleTTest", position = 2)
  
  # Create the posterior probability plots
  .bainPosteriorProbabilityPlot(dataList[["dataset"]], options, bainContainer, ready, type = "onesampleTTest", position = 3)
  
  ### DESCRIPTIVES PLOTS ###
  .bainOneSampleDescriptivesPlot(dataList[["dataset"]], options, bainContainer, ready, position = 4)
}

.bainOneSampleDescriptivesPlot <- function(dataset, options, bainContainer, ready, position) {
  
  if (!is.null(bainContainer[["descriptivesPlots"]]) || !options[["descriptivesPlots"]]) return()
  
  descriptivesPlots <- createJaspContainer(gettext("Descriptive Plots"))
  descriptivesPlots$dependOn(options =c("variables", "descriptivesPlots", "credibleInterval"))
  descriptivesPlots$position <- position
  
  bainContainer[["descriptivesPlots"]] <- descriptivesPlots
  
  if (!ready)
    return()
  
  for (variable in options[["variables"]]) {
    
    if(is.null(bainContainer[["descriptivesPlots"]][[variable]])){
      
      bainAnalysis <- .bainAnalysisState(dataset, options, bainContainer, ready, type = "onesampleTTest", variable = variable)
      
      if(isTryError(bainAnalysis)){
        
        descriptivesPlots[[variable]] <- createJaspPlot(plot=NULL, title = variable)
        descriptivesPlots[[variable]]$dependOn(optionContainsValue=list("variables" = variable))
        descriptivesPlots[[variable]]$setError(.extractErrorMessage(bainAnalysis))
        
      } else {
        
        bainSummary <- summary(bainAnalysis, ci = options[["credibleInterval"]])
        
        # Descriptive statistics from bain
        N <- bainSummary[["n"]]
        mu <- bainSummary[["Estimate"]]
        CiLower <- bainSummary[["lb"]]
        CiUpper <- bainSummary[["ub"]]
        
        yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(options[["testValue"]], CiLower, CiUpper), min.n = 4)
        d <- data.frame(v = variable, N = N, mean = mu, lowerCI = CiLower, upperCI = CiUpper, index = 1)
        
        p <- ggplot2::ggplot(d, ggplot2::aes(x=index, y=mean)) +
          ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = Inf, y = options[["testValue"]], yend = options[["testValue"]]), linetype = 2, color = "darkgray") +
          ggplot2::geom_errorbar(ggplot2::aes(ymin=lowerCI, ymax=upperCI), colour="black", width=.1, position = ggplot2::position_dodge(.2)) +
          ggplot2::geom_point(position=ggplot2::position_dodge(.2), size=4) +
          ggplot2::ylab("") +
          ggplot2::xlab("") +
          ggplot2::scale_y_continuous(breaks = yBreaks, labels = yBreaks, limits = range(yBreaks)) +
          ggplot2::scale_x_continuous(breaks = 0:2, labels = NULL)
        p <- jaspGraphs::themeJasp(p, xAxis = FALSE) + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
        
        descriptivesPlots[[variable]] <- createJaspPlot(plot=p, title = variable)
        descriptivesPlots[[variable]]$dependOn(optionContainsValue=list("variables" = variable))
      }
    }
  }
}