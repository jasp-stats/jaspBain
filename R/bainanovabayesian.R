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

BainAnovaBayesian <- function(jaspResults, dataset, options, ...) {
  
  # Check if current options allow for analysis
  ready <- .bainOptionsReady(options, type = "anova")
  
  # Read the data set
  dataList <- .bainReadDataset(options, type = "anova", dataset)
  
  # Check if current data allow for analysis
  .bainDataReady(dataList[["dataset"]], options, type = "anova")
  
  # Create a container for the results
  bainContainer <- .bainGetContainer(jaspResults, deps = c("dependent", "fixedFactors", "model"))
  
  # Create a legend containing the order constrained hypotheses
  .bainLegend(dataList[["dataset"]], options, type = "anova", jaspResults, position = 0)
  
  # Create a table containing the main analysis results
  .bainResultsTable(dataList[["dataset"]], options, bainContainer, dataList[["missing"]], ready, type = "anova", position = 1)
  
  # Create the Bayes factor matrix
  .bainBfMatrix(dataList[["dataset"]], options, bainContainer, ready, type = "anova", position = 2)
  
  # Create the descriptive statistics table
  .bainDescriptivesTable(dataList[["dataset"]], options, bainContainer, ready, type = "anova", position = 3)
  
  ### POSTERIOR PROBABILITIES PLOT ###
  .bainAnovaBayesFactorPlots(dataList[["dataset"]], options, bainContainer, ready, position = 4)
  
  ### DESCRIPTIVES PLOT ###
  .bainAnovaDescriptivesPlot(dataList[["dataset"]], options, bainContainer, ready, type = "anova", position = 5)
}

.bainAnovaDescriptivesTable <- function(dataset, options, bainContainer, ready, type = "anova", position) {
  
  if (!is.null(bainContainer[["descriptivesTable"]]) || !options[["descriptives"]]) return()
  
  title     <- ifelse(type == "anova", yes = gettext("Descriptive Statistics"), no = gettext("Coefficients for Groups plus Covariates"))
  meanTitle <- ifelse(type == "anova", yes = gettext("Mean"),                   no = gettext("Coefficient"))
  
  descriptivesTable <- createJaspTable(title)
  descriptivesTable$dependOn(options =c("descriptives", "credibleInterval", "coefficients"))
  descriptivesTable$position <- position
  
  descriptivesTable$addColumnInfo(name="v",    		title="",         	 type="string")
  descriptivesTable$addColumnInfo(name="N",    		title=gettext("N"),	 type="integer")
  descriptivesTable$addColumnInfo(name="mean", 		title=meanTitle,		 type="number")
  if(type == "anova")
    descriptivesTable$addColumnInfo(name="sd", 		title=gettext("SD"),	type="number")
  descriptivesTable$addColumnInfo(name="se",   		title=gettext("SE"), 	type="number")
  
  overTitle <- gettextf("%.0f%% Credible Interval", options[["credibleInterval"]] * 100)
  descriptivesTable$addColumnInfo(name="lowerCI",      title = gettext("Lower"), type="number", overtitle = overTitle)
  descriptivesTable$addColumnInfo(name="upperCI",      title = gettext("Upper"), type="number", overtitle = overTitle)
  
  bainContainer[["descriptivesTable"]] <- descriptivesTable
  
  if (!ready || bainContainer$getError())
    return()
  
  groupCol <- dataset[ , .v(options[["fixedFactors"]])]
  varLevels <- levels(groupCol)
  
  bainResult <- bainContainer[["bainResult"]]$object
  bainSummary <- summary(bainResult, ci = options[["credibleInterval"]])
  sigma <- diag(bainResult$posterior)
  
  # Extract all but sd and se from bain result
  variable <- .unv(bainSummary[["Parameter"]])
  N        <- bainSummary[["n"]]
  mu       <- bainSummary[["Estimate"]]
  CiLower  <- bainSummary[["lb"]]
  CiUpper  <- bainSummary[["ub"]]
  
  if(type == "anova"){
    # Include the standard deviation from the groups
    sd <- aggregate(dataset[, .v(options[["dependent"]])], list(dataset[, .v(options[["fixedFactors"]])]), sd)[, 2]
  }
  se <- sqrt(sigma)	
  
  row <- data.frame(v = variable, N = N, mean = mu, se = se, lowerCI = CiLower, upperCI = CiUpper)
  if(type == "anova")
    row <- cbind(row, sd = sd)
  descriptivesTable$addRows(row)
}

.bainAnovaBayesFactorPlots <- function(dataset, options, bainContainer, ready, position) {
  
  if (!is.null(bainContainer[["bayesFactorPlot"]]) || !options[["bayesFactorPlot"]]) return()
  
  if(options[["model"]] == ""){
    height <- 300
    width <- 400
  } else {
    height <- 400
    width <- 600
  }
  
  bayesFactorPlot <- createJaspPlot(plot = NULL, title = gettext("Posterior Probabilities"), height = height, width = width)
  
  bayesFactorPlot$dependOn(options=c("bayesFactorPlot", "seed"))
  bayesFactorPlot$position <- position
  
  bainContainer[["bayesFactorPlot"]] <- bayesFactorPlot
  
  if (!ready || bainContainer$getError())
    return()
  
  bainResult <- bainContainer[["bainResult"]]$object
  bayesFactorPlot$plotObject <- .suppressGrDevice(.plot_bain_ancova_cran(bainResult))
}

.bainAnovaDescriptivesPlot <- function(dataset, options, bainContainer, ready, type = "anova", position) {
  
  if (!is.null(bainContainer[["descriptivesPlot"]]) || !options[["descriptivesPlot"]]) return()
  
  plotTitle <- ifelse(type == "anova", yes = gettext("Descriptives Plot"), no = gettext("Adjusted Means"))
  
  descriptivesPlot <- createJaspPlot(plot = NULL, title = plotTitle)
  descriptivesPlot$dependOn(options=c("descriptivesPlot", "credibleInterval"))
  descriptivesPlot$position <- position
  
  bainContainer[["descriptivesPlot"]] <- descriptivesPlot
  
  if (!ready || bainContainer$getError())
    return()
  
  bainBreaks <- function(x, plotErrorBars = TRUE) {
    ci.pos <- c(x[,"mean"], x[,"lowerCI"], x[,"upperCI"])
    b <- pretty(ci.pos)
    d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
    list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE, size = 1),
         ggplot2::scale_y_continuous(breaks=c(min(b),max(b))))
  }
  
  groupCol <- dataset[ , .v(options[["fixedFactors"]])]
  varLevels <- levels(groupCol)
  
  bainResult <- bainContainer[["bainResult"]]$object
  bainSummary <- summary(bainResult, ci = options[["credibleInterval"]])
  
  # Remove covariates in ANCOVA
  if(type == "ancova")
    bainSummary <- bainSummary[1:length(varLevels), ]
  
  # Extract all but sd and se from bain result
  variable <- bainSummary[["Parameter"]]
  N <- bainSummary[["n"]]
  mu <- bainSummary[["Estimate"]]
  CiLower <- bainSummary[["lb"]]
  CiUpper <- bainSummary[["ub"]]
  
  d <- data.frame(v = variable, N = N, mean = mu, lowerCI = CiLower, upperCI = CiUpper, index = 1:length(variable))
  
  p <- ggplot2::ggplot(d, ggplot2::aes(x=index, y=mean)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=lowerCI, ymax=upperCI), colour="black", width=.2, position = ggplot2::position_dodge(.2)) +
    ggplot2::geom_line(position=ggplot2::position_dodge(.2), size = .7) +
    ggplot2::geom_point(position=ggplot2::position_dodge(.2), size=4) +
    ggplot2::scale_fill_manual(values = c(rep(c("white","black"),5),rep("grey",100)), guide=ggplot2::guide_legend(nrow=10)) +
    ggplot2::scale_shape_manual(values = c(rep(c(21:25),each=2),21:25,7:14,33:112), guide=ggplot2::guide_legend(nrow=10)) +
    ggplot2::scale_color_manual(values = rep("black",200), guide=ggplot2::guide_legend(nrow=10)) +
    ggplot2::ylab(options[["dependent"]]) +
    ggplot2::xlab(options[["fixedFactors"]]) +
    bainBreaks(d, TRUE) +
    ggplot2::scale_x_continuous(breaks = 1:length(varLevels), labels = as.character(varLevels))
  p <- jaspGraphs::themeJasp(p)
  
  descriptivesPlot$plotObject <- p
}
