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

BainTTestBayesianIndependentSamples <- function(jaspResults, dataset, options, ...) {
  
  # Check if current options allow for analysis
  ready <- .bainOptionsReady(options, type = "independentTTest")
  
  # Read the data set 
  dataList <- .bainReadDataset(options, type = "independentTTest", dataset)
  
  # Check if current data allow for analysis
  .bainDataReady(dataList[["dataset"]], options, type = "independentTTest")
  
  # Create a container for the results
  bainContainer <- .bainGetContainer(jaspResults, deps = c("groupingVariable", "seed"))
  
  # Create a table containing the main analysis results
  .bainResultsTable(dataList[["dataset"]], options, bainContainer, dataList[["missing"]], ready, type = "independentTTest", position = 1)
  
  ### DESCRIPTIVES ###
  .bainIndependentSamplesDescriptivesTable(dataList[["dataset"]], options, bainContainer, ready, position = 2)
  
  ### POSTERIOR PROBABILITIES PLOT ###
  .bainTTestFactorPlots(dataList[["dataset"]], options, bainContainer, ready, type = "independentSamples", position = 3)
  
  ### DESCRIPTIVES PLOTS ###
  .bainIndependentSamplesDescriptivesPlots(dataList[["dataset"]], options, bainContainer, ready, position = 4)
}

.bainIndependentSamplesDescriptivesTable <- function(dataset, options, bainContainer, ready, position) {
  
  if (!is.null(bainContainer[["descriptivesTable"]]) || !options[["descriptives"]]) return() 
  
  descriptivesTable <- createJaspTable(gettext("Descriptive Statistics"))
  descriptivesTable$dependOn(options =c("variables", "descriptives", "credibleInterval"))
  descriptivesTable$position <- position
  
  descriptivesTable$addColumnInfo(name="v",                    title = "",               type="string")
  descriptivesTable$addColumnInfo(name="group",                title = gettext("Group"), type="string")
  descriptivesTable$addColumnInfo(name="N",                    title = gettext("N"),     type="integer")
  descriptivesTable$addColumnInfo(name="mean",                 title = gettext("Mean"),  type="number")
  descriptivesTable$addColumnInfo(name="sd",                   title = gettext("SD"),    type="number")
  descriptivesTable$addColumnInfo(name="se",                   title = gettext("SE"),    type="number")
  
  overTitle <- gettextf("%.0f%% Credible Interval", 100 * options[["credibleInterval"]])
  descriptivesTable$addColumnInfo(name="lowerCI",              title = gettext("Lower"), type="number", overtitle = overTitle)
  descriptivesTable$addColumnInfo(name="upperCI",              title = gettext("Upper"), type="number", overtitle = overTitle)
  
  bainContainer[["descriptivesTable"]] <- descriptivesTable
  
  if (!ready)
    return()
  
  descriptivesTable$setExpectedSize(length(options[["variables"]]) * 2)
  
  levels <- base::levels(dataset[[ .v(options[["groupingVariable"]]) ]])
  if (length(levels) != 2) {
    g1 <- "1"
    g2 <- "2"
  } else {
    g1 <- levels[1]
    g2 <- levels[2]
  }
  
  for (variable in options[["variables"]]) {
    
    bainAnalysis <- .bainAnalysisState(dataset, options, bainContainer, ready, type = "independentTTest", variable = variable)
    
    if(isTryError(bainAnalysis)){
      
      descriptivesTable$addRows(data.frame(v=variable), rowNames=variable)
      descriptivesTable$addFootnote(message=gettextf("Results not computed: %s", .extractErrorMessage(bainAnalysis)), colNames="v", rowNames=variable)
      
    } else {
      
      bainSummary <- summary(bainAnalysis, ci = options[["credibleInterval"]])
      
      # Descriptives from bain, sd calculated manually
      N <- bainSummary[["n"]]
      mu <- bainSummary[["Estimate"]]
      CiLower <- bainSummary[["lb"]]
      CiUpper <- bainSummary[["ub"]]
      sd <- aggregate(dataset[, .v(variable)], list(dataset[, .v(options[["groupingVariable"]])]), sd)[, 2]
      se <- sqrt(diag(bainAnalysis[["posterior"]]))
      
      row <- data.frame(v = variable, group = g1, N = N[1], mean = mu[1], sd = sd[1], se = se[1], lowerCI = CiLower[1], upperCI = CiUpper[1])
      descriptivesTable$addRows(row)
      row <- data.frame(v = "", group = g2, N = N[2], mean = mu[2], sd = sd[2], se = se[2], lowerCI = CiLower[2], upperCI = CiUpper[2])
      descriptivesTable$addRows(row)
      
    }
  }
}

.bainIndependentSamplesDescriptivesPlots <- function(dataset, options, bainContainer, ready, position) {
  
  if (!is.null(bainContainer[["descriptivesPlots"]]) || !options[["descriptivesPlots"]]) return()
  
  descriptivesPlots <- createJaspContainer(gettext("Descriptive Plots"))
  descriptivesPlots$dependOn(options = c("variables", "descriptivesPlots", "credibleInterval"))
  descriptivesPlots$position <- position
  
  bainContainer[["descriptivesPlots"]] <- descriptivesPlots
  
  if (!ready)
    return()
  
  for (variable in options[["variables"]]) {
    
    if (is.null(bainContainer[["descriptivesPlots"]][[variable]])){
      
      bainAnalysis <- .bainAnalysisState(dataset, options, bainContainer, ready, type = "independentTTest", variable = variable)
      
      if(isTryError(bainAnalysis)){
        
        descriptivesPlots[[variable]] <- createJaspPlot(plot=NULL, title = variable)
        descriptivesPlots[[variable]]$dependOn(optionContainsValue=list("variables" = variable))
        descriptivesPlots[[variable]]$setError(gettextf("Results not computed: %s", .extractErrorMessage(bainAnalysis)))
        
      } else {
        
        bainSummary <- summary(bainAnalysis, ci = options[["credibleInterval"]])
        levels <- base::levels(dataset[[ .v(options[["groupingVariable"]]) ]])
        
        # Descriptive statistics from bain
        N <- bainSummary[["n"]]
        mu <- bainSummary[["Estimate"]]
        CiLower <- bainSummary[["lb"]]
        CiUpper <- bainSummary[["ub"]]
        
        yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(CiLower, CiUpper), min.n = 4)
        d <- data.frame(v = levels, N = N, mean = mu, lowerCI = CiLower, upperCI = CiUpper, index = 1:length(levels))
        
        p <- ggplot2::ggplot(d, ggplot2::aes(x=index, y=mean)) +
          ggplot2::geom_errorbar(ggplot2::aes(ymin=lowerCI, ymax=upperCI), colour="black", width=.2, position = ggplot2::position_dodge(.2)) +
          ggplot2::geom_line(position=ggplot2::position_dodge(.2), size = .7) +
          ggplot2::geom_point(position=ggplot2::position_dodge(.2), size=4) +
          ggplot2::ylab(variable) +
          ggplot2::xlab(options[["groupingVariable"]]) +
          ggplot2::scale_x_continuous(breaks = 1:length(levels), labels = as.character(levels)) +
          ggplot2::scale_y_continuous(breaks = yBreaks, labels = yBreaks, limits = range(yBreaks))
        p <- jaspGraphs::themeJasp(p)
        
        descriptivesPlots[[variable]] <- createJaspPlot(plot=p, title = variable)
        descriptivesPlots[[variable]]$dependOn(optionContainsValue=list("variables" = variable))
      }  
    }
  }
}
