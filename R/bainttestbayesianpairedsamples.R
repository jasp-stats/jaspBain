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

BainTTestBayesianPairedSamples <- function(jaspResults, dataset, options, ...) {
  
  # Check if current options allow for analysis
  ready <- .bainOptionsReady(options, type = "pairedTTest")
  
  # Read the data set
  dataList <- .bainReadDataset(options, type = "pairedTTest", dataset)
  
  # Check if current data allow for analysis
  .bainDataReady(dataset, options, type = "pairedTTest")
  
  # Create a container for the results
  bainContainer <- .bainGetContainer(jaspResults, deps = "seed")
  
  # Create a table containing the main analysis results
  .bainResultsTable(dataList[["dataset"]], options, bainContainer, dataList[["missing"]], ready, type = "pairedTTest", position = 1)
  
  # Create the descriptive statistics table
  .bainDescriptivesTable(dataList[["dataset"]], options, bainContainer, ready, type = "pairedTTest", position = 2)
  
  ### POSTERIOR PROBABILITIES PLOT ###
  .bainTTestFactorPlots(dataList[["dataset"]], options, bainContainer, ready, type = "pairedSamples", position = 3)
  
  ### DESCRIPTIVES PLOTS ###
  .bainPairedSamplesDescriptivesPlots(dataList[["dataset"]], options, bainContainer, ready, position = 4)
}

.bainPairedSamplesDescriptivesTable <- function(dataset, options, bainContainer, ready, position) {
  
  if (!is.null(bainContainer[["descriptivesTable"]]) || !options[["descriptives"]]) return() 
  
  descriptivesTable <- createJaspTable(gettext("Descriptive Statistics"))
  descriptivesTable$dependOn(options =c("pairs", "descriptives", "credibleInterval"))
  descriptivesTable$position <- position
  
  descriptivesTable$addColumnInfo(name="v",                    title = "",              type="string")
  descriptivesTable$addColumnInfo(name="N",                    title = gettext("N"),    type="integer")
  descriptivesTable$addColumnInfo(name="mean",                 title = gettext("Mean"), type="number")
  descriptivesTable$addColumnInfo(name="sd",                   title = gettext("SD"),   type="number")
  descriptivesTable$addColumnInfo(name="se",                   title = gettext("SE"),   type="number")
  
  overTitle <- gettextf("%.0f%% Credible Interval", 100 * options[["credibleInterval"]])
  descriptivesTable$addColumnInfo(name="lowerCI",              title = gettext("Lower"), type="number", overtitle = overTitle)
  descriptivesTable$addColumnInfo(name="upperCI",              title = gettext("Upper"), type="number", overtitle = overTitle)
  
  bainContainer[["descriptivesTable"]] <- descriptivesTable
  
  if (!ready)
    return()
  
  descriptivesTable$setExpectedSize(length(options[["pairs"]]))
  
  for (pair in options[["pairs"]]) {
    
    if (pair[[2]] != "" && pair[[1]] != pair[[2]]) {
      
      subDataSet <- subset(dataset, select=c(.v(pair[[1]]), .v(pair[[2]])))
      
      c1 <- subDataSet[[ .v(pair[[1]]) ]]
      c2 <- subDataSet[[ .v(pair[[2]]) ]]
      difference <- c1 - c2
      
      currentPair <- paste(pair, collapse=" - ")
          testType <- base::switch(options[["hypothesis"]],
                             "equalNotEqual"       = 1,
                             "equalBigger"         = 2,
                             "equalSmaller"        = 3,
                             "biggerSmaller"       = 4,
                             "equalBiggerSmaller"  = 5)
      bainAnalysis <- .bainAnalysisState(dataset, options, bainContainer, ready, type = "pairedTTest", pair = pair, testType = testType)
      
      if(isTryError(bainAnalysis)){
        
        descriptivesTable$addRows(data.frame(v=currentPair), rowNames=currentPair)
        descriptivesTable$addFootnote(message=gettextf("Results not computed: %s", .extractErrorMessage(bainAnalysis)), colNames="v", rowNames=currentPair)
        
      } else {
        
        bainSummary <- summary(bainAnalysis, ci = options[["credibleInterval"]])
        
        # Descriptive statistics from bain, sd calculated manually
        N <- bainSummary[["n"]]
        mu <- bainSummary[["Estimate"]]
        CiLower <- bainSummary[["lb"]]
        CiUpper <- bainSummary[["ub"]]
        se <- sqrt(diag(bainAnalysis[["posterior"]]))
        sd <- sd(difference)
        
        row <- list(v = currentPair, N = N, mean = mu, sd = sd, se = se, lowerCI = CiLower, upperCI = CiUpper)
        descriptivesTable$addRows(row)
      }
    }
  }
}

.bainPairedSamplesDescriptivesPlots <- function(dataset, options, bainContainer, ready, position) {
  
  if (!is.null(bainContainer[["descriptivesPlots"]]) || !options[["descriptivesPlots"]]) return()
  
  descriptivesPlots <- createJaspContainer(gettext("Descriptive Plots"))
  descriptivesPlots$dependOn(options =c("pairs", "descriptivesPlots", "credibleInterval"))
  descriptivesPlots$position <- position
  
  bainContainer[["descriptivesPlots"]] <- descriptivesPlots
  
  if (!ready)
    return()
  
  for (pair in options[["pairs"]]) {
    
    currentPair <- paste(pair, collapse=" - ")
    
    if (is.null(bainContainer[["descriptivesPlots"]][[currentPair]]) && pair[[2]] != "" && pair[[1]] != pair[[2]]){
      
                testType <- base::switch(options[["hypothesis"]],
                             "equalNotEqual"       = 1,
                             "equalBigger"         = 2,
                             "equalSmaller"        = 3,
                             "biggerSmaller"       = 4,
                             "equalBiggerSmaller"  = 5)
      bainAnalysis <- .bainAnalysisState(dataset, options, bainContainer, ready, type = "pairedTTest", pair = pair, testType = testType)
      
      if(isTryError(bainAnalysis)){
        
        descriptivesPlots[[currentPair]] <- createJaspPlot(plot=NULL, title = currentPair)
        descriptivesPlots[[currentPair]]$dependOn(optionContainsValue=list("variables" = currentPair))
        descriptivesPlots[[currentPair]]$setError(.extractErrorMessage(bainAnalysis))
        
      } else {
        
        bainSummary <- summary(bainAnalysis, ci = options[["credibleInterval"]])
        
        # Descriptive statistics from bain
        N <- bainSummary[["n"]]
        mu <- bainSummary[["Estimate"]]
        CiLower <- bainSummary[["lb"]]
        CiUpper <- bainSummary[["ub"]]
        
        yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, CiLower, CiUpper), min.n = 4)
        d <- data.frame(v = gettext("Difference"), N = N, mean = mu, lowerCI = CiLower, upperCI = CiUpper, index = 1)
        
        p <- ggplot2::ggplot(d, ggplot2::aes(x=index, y=mean)) +
          ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = Inf, y = 0, yend = 0), linetype = 2, color = "darkgray") +
          ggplot2::geom_errorbar(ggplot2::aes(ymin=lowerCI, ymax=upperCI), colour="black", width=.1, position = ggplot2::position_dodge(.2)) +
          ggplot2::geom_point(position=ggplot2::position_dodge(.2), size=4) +
          ggplot2::ylab("") +
          ggplot2::xlab("") +
          ggplot2::scale_y_continuous(breaks = yBreaks, labels = yBreaks, limits = range(yBreaks)) +
          ggplot2::scale_x_continuous(breaks = 0:2, labels = NULL)
        p <- jaspGraphs::themeJasp(p, xAxis = FALSE) + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
        
        descriptivesPlots[[currentPair]] <- createJaspPlot(plot=p, title = currentPair)
        descriptivesPlots[[currentPair]]$dependOn(optionContainsValue=list("pairs" = pair))
      }
    }
  }
}
