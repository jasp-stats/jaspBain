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
  ready 	<- .bainOptionsReady(options, type = "onesampleTTest")
  
  # Read the data set 
  dataList 	<- .bainReadDataset(options, type = "onesampleTTest", dataset)
  
  # Check if current data allow for analysis
  .bainDataReady(dataset, options, type = "onesampleTTest")
  
  # Create a container for the results
  bainContainer <- .bainGetContainer(jaspResults, deps = c("testValue", "seed"))
  
  ### RESULTS ###
  .bainOneSampleResultsTable(dataList[["dataset"]], options, bainContainer, dataList[["missing"]], ready, position = 1)
  
  ### DESCRIPTIVES ###
  .bainOneSampleDescriptivesTable(dataList[["dataset"]], options, bainContainer, ready, position = 2)
  
  ### POSTERIOR PROBABILITIES PLOT ###
  .bainTTestFactorPlots(dataList[["dataset"]], options, bainContainer, ready, type = "oneSample", position = 3)
  
  ### DESCRIPTIVES PLOTS ###
  .bainOneSampleDescriptivesPlot(dataList[["dataset"]], options, bainContainer, ready, position = 4)
}

.bainOneSampleState <- function(variable, options, dataset, bainContainer){
  
  if(!is.null(bainContainer[[variable]]))
    return(bainContainer[[variable]]$object)
  
  type <- base::switch(options[["hypothesis"]],
                       "equalNotEqual"       = 1,
                       "equalBigger"         = 2,
                       "equalSmaller"        = 3,
                       "biggerSmaller"       = 4,
                       "equalBiggerSmaller"  = 5)
  
  variableData <- dataset[[ .v(variable) ]]
  testValue <- format(options[["testValue"]], scientific = FALSE)
  
  p <- try({
    # Call bain from package
    bain:::bain_ttest_cran(x = variableData, nu = testValue, type = type, seed = options[["seed"]])
  })
  bainContainer[[variable]] <- createJaspState(p, dependencies = c("testValue", "hypothesis", "seed"))
  bainContainer[[variable]]$dependOn(optionContainsValue=list("variables" = variable))
  
  return(bainContainer[[variable]]$object)
}

.bainOneSampleResultsTable <- function(dataset, options, bainContainer, missingValuesIndicator, ready, position) {
  
  if (!is.null(bainContainer[["bainTable"]])) return()
  
  bainTable <- createJaspTable(gettext("Bain One Sample T-test"))
  bainTable$position <- position
  bainTable$dependOn(options = c("variables", "hypothesis", "bayesFactorType"))
  
  bf.type <- options[["bayesFactorType"]]
  BFH1H0 <- FALSE
  bf.title <- gettext("BF")
  
  if (options$hypothesis == "equalBiggerSmaller") {
    bainTable$addColumnInfo(name="Variable",      type="string",                title="")
    bainTable$addColumnInfo(name="type[equal]",   type="string",                title=gettext("Hypothesis"))
    bainTable$addColumnInfo(name="BF[equal]",     type="number",                title=bf.title)
    bainTable$addColumnInfo(name="pmp[equal]",    type="number", format="dp:3", title=gettext("Posterior probability"))
    bainTable$addColumnInfo(name="type[greater]", type="string",                title=gettext("Hypothesis"))
    bainTable$addColumnInfo(name="BF[greater]",   type="number",                title=bf.title)
    bainTable$addColumnInfo(name="pmp[greater]",  type="number", format="dp:3", title=gettext("Posterior probability"))
    bainTable$addColumnInfo(name="type[less]",    type="string",                title=gettext("Hypothesis"))
    bainTable$addColumnInfo(name="BF[less]",      type="number",                title=bf.title)
    bainTable$addColumnInfo(name="pmp[less]",     type="number", format="dp:3", title=gettext("Posterior probability"))
  } else {
    bainTable$addColumnInfo(name="Variable",          type="string",                title="")
    bainTable$addColumnInfo(name="hypothesis[type1]", type="string",                title=gettext("Hypothesis"))
    bainTable$addColumnInfo(name="BF[type1]",         type="number",                title=bf.title)
    bainTable$addColumnInfo(name="pmp[type1]",        type="number", format="dp:3", title=gettext("Posterior probability"))
    bainTable$addColumnInfo(name="hypothesis[type2]", type="string",                title=gettext("Hypothesis"))
    bainTable$addColumnInfo(name="BF[type2]",         type="number",                title=bf.title)
    bainTable$addColumnInfo(name="pmp[type2]",        type="number", format="dp:3", title=gettext("Posterior probability"))
  }
  
  type <- base::switch(options[["hypothesis"]],
                       "equalNotEqual"       = 1,
                       "equalBigger"         = 2,
                       "equalSmaller"        = 3,
                       "biggerSmaller"       = 4,
                       "equalBiggerSmaller"  = 5)
  message <- base::switch(options[["hypothesis"]],
                          "equalNotEqual"     = gettextf("The alternative hypothesis H1 specifies that the mean is unequal to %s. The posterior probabilities are based on equal prior probabilities.", options[["testValue"]]),
                          "equalBigger"       = gettextf("The alternative hypothesis H1 specifies that the mean is bigger than %s. The posterior probabilities are based on equal prior probabilities.", options[["testValue"]]),
                          "equalSmaller"      = gettextf("The alternative hypothesis H1 specifies that the mean is smaller than %s. The posterior probabilities are based on equal prior probabilities.", options[["testValue"]]),
                          "biggerSmaller"     = gettextf("The hypothesis H1 specifies that the mean is bigger than %1$s and the hypothesis H2 specifies that the mean is smaller than %1$s. The posterior probabilities are based on equal prior probabilities.", options[["testValue"]]),
                          "equalBiggerSmaller"= gettextf("The null hypothesis H0 with test value %1$s is tested against the other hypotheses. H1 states that the mean is bigger than %1$s and H2 states that the mean is smaller than %1$s. The posterior probabilities are based on equal prior probabilities.", options[["testValue"]]))
  bainTable$addFootnote(message=message)
  
  bainTable$addCitation(.bainGetCitations())
  
  bainContainer[["bainTable"]] <- bainTable
  
  if (!ready)
    return()
  
  bainTable$setExpectedSize(length(options[["variables"]]))
  
  startProgressbar(length(options[["variables"]]))
  
  for (variable in options[["variables"]]) {
    
    bainAnalysis <- .bainOneSampleState(variable, options, dataset, bainContainer)
    
    if (isTryError(bainAnalysis)){
      bainTable$addRows(list(Variable=variable), rowNames=variable)
      bainTable$addFootnote(message=paste0("Results not computed: ", .extractErrorMessage(bainAnalysis)), colNames="Variable", rowNames=variable)
      progressbarTick()
      next
    }
    
    if (variable %in% missingValuesIndicator) {
      bainTable$addFootnote(message= paste0("Variable contains missing values, the rows containing these values are removed in the analysis."), colNames="Variable", rowNames=variable)
    }
    
    if (type == 1) {
      BF_0u <- bainAnalysis$fit$BF[1]
      PMP_u <- bainAnalysis$fit$PMPb[2]
      PMP_0 <- bainAnalysis$fit$PMPb[1]
      if (options$bayesFactorType == "BF10")
        BF_0u <- 1/BF_0u
    }
    if (type == 2) {
      BF_01 <- bainAnalysis$BFmatrix[1,2]
      PMP_1 <- bainAnalysis$fit$PMPa[2]
      PMP_0 <- bainAnalysis$fit$PMPa[1]
      if (options$bayesFactorType == "BF10")
        BF_01 <- 1/BF_01
    }
    if (type == 3) {
      BF_01 <- bainAnalysis$BFmatrix[1,2]
      PMP_0 <- bainAnalysis$fit$PMPa[1]
      PMP_1 <- bainAnalysis$fit$PMPa[2]
      if (options$bayesFactorType == "BF10")
        BF_01 <- 1/BF_01
    }
    if (type == 4) {
      BF_01 <- bainAnalysis$BFmatrix[1,2]
      PMP_0 <- bainAnalysis$fit$PMPa[1]
      PMP_1 <- bainAnalysis$fit$PMPa[2]
      if (options$bayesFactorType == "BF10")
        BF_01 <- 1/BF_01
    }
    if (type == 5) {
      BF_01 <- bainAnalysis$BFmatrix[1,2]
      BF_02 <- bainAnalysis$BFmatrix[1,3]
      BF_12 <- bainAnalysis$BFmatrix[2,3]
      PMP_0 <- bainAnalysis$fit$PMPa[1]
      PMP_1 <- bainAnalysis$fit$PMPa[2]
      PMP_2 <- bainAnalysis$fit$PMPa[3]
      if (options$bayesFactorType == "BF10")
      {
        BF_01 <- 1/BF_01
        BF_02 <- 1/BF_02
        BF_12 <- 1/BF_12
      }
    }
    
    # The code here looks suspiciously similar to the code in BAIN TTest Independent Samples, perhaps this could be merged?
    
    if (options$bayesFactorType == "BF01") {
      if (options$hypothesis == "equalNotEqual") {
        row <- list(Variable=variable, "hypothesis[type1]" = gettext("H0: Equal"),"BF[type1]"=BF_0u, "pmp[type1]" = PMP_0,
                    "hypothesis[type2]" = gettext("H1: Not equal"), "BF[type2]" = "", "pmp[type2]" = PMP_u)
      } else if (options$hypothesis == "equalBigger") {
        row <-list(Variable=variable, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]"=BF_01, "pmp[type1]" = PMP_0,
                   "hypothesis[type2]" = gettext("H1: Bigger"), "BF[type2]" = "", "pmp[type2]" = PMP_1)
      } else if (options$hypothesis == "equalSmaller") {
        row <-list(Variable=variable, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]"=BF_01, "pmp[type1]" = PMP_0,
                   "hypothesis[type2]" = gettext("H1: Smaller"), "BF[type2]" = "", "pmp[type2]" = PMP_1)
      } else if (options$hypothesis == "biggerSmaller") {
        row <-list(Variable=variable, "hypothesis[type1]" = gettext("H1: Bigger"), "BF[type1]"=BF_01, "pmp[type1]" = PMP_0,
                   "hypothesis[type2]" = gettext("H2: Smaller"), "BF[type2]" = "", "pmp[type2]" = PMP_1)
      } else if (options$hypothesis == "equalBiggerSmaller") {
        row <-list(Variable=variable, "type[equal]" = gettext("H0: Equal"), "BF[equal]"= "", "pmp[equal]" = PMP_0,
                   "type[greater]" =gettext( "H1: Bigger"), "BF[greater]" = BF_01, "pmp[greater]" = PMP_1,
                   "type[less]" = gettext("H2: Smaller"), "BF[less]" = BF_02, "pmp[less]" = PMP_2)
      }
    } else if (options$bayesFactorType == "BF10") {
      if (options$hypothesis == "equalNotEqual") {
        row <- list(Variable=variable, "hypothesis[type1]" = gettext("H0: Equal"),"BF[type1]"="", "pmp[type1]" = PMP_0,
                    "hypothesis[type2]" = gettext("H1: Not equal"), "BF[type2]" = BF_0u, "pmp[type2]" = PMP_u)
      } else if (options$hypothesis == "equalBigger") {
        row <-list(Variable=variable, "hypothesis[type1]" = gettext("H0: Equal"),"BF[type1]"="", "pmp[type1]" = PMP_0,
                   "hypothesis[type2]" = gettext("H1: Bigger"), "BF[type2]" = BF_01, "pmp[type2]" = PMP_1)
      } else if (options$hypothesis == "equalSmaller") {
        row <-list(Variable=variable, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]"="", "pmp[type1]" = PMP_0,
                   "hypothesis[type2]" = gettext("H1: Smaller"), "BF[type2]" = BF_01, "pmp[type2]" = PMP_1)
      } else if (options$hypothesis == "biggerSmaller") {
        row <-list(Variable=variable, "hypothesis[type1]" = gettext("H1: Bigger"), "BF[type1]"= "", "pmp[type1]" = PMP_0,
                   "hypothesis[type2]" = gettext("H2: Smaller"), "BF[type2]" = BF_01, "pmp[type2]" = PMP_1)
      } else if (options$hypothesis == "equalBiggerSmaller") {
        row <-list(Variable=variable, "type[equal]" = gettext("H0: Equal"), "BF[equal]"= "", "pmp[equal]" = PMP_0,
                   "type[greater]"= gettext("H1: Bigger"), "BF[greater]" = BF_01, "pmp[greater]" = PMP_1,
                   "type[less]" = gettext("H2: Smaller"), "BF[less]" = BF_02, "pmp[less]" = PMP_2)
      }
    }
    bainTable$addRows(row, rowNames = variable)
    progressbarTick()
  }
}

.bainOneSampleDescriptivesTable <- function(dataset, options, bainContainer, ready, position) {
  
  if (!is.null(bainContainer[["descriptivesTable"]]) || !options[["descriptives"]]) return()
  
  descriptivesTable <- createJaspTable(gettext("Descriptive Statistics"))
  descriptivesTable$dependOn(options = c("variables", "descriptives", "credibleInterval"))
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
  
  descriptivesTable$setExpectedSize(length(options[["variables"]]))
  
  for (variable in options[["variables"]]) {
    
    bainAnalysis <- .bainOneSampleState(variable, options, dataset, bainContainer)
    
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
      sd <- sd(dataset[, .v(variable)])
      se <- sqrt(diag(bainAnalysis[["posterior"]]))
      
      row <- list(v = variable, N = N, mean = mu, sd = sd, se = se, lowerCI = CiLower, upperCI = CiUpper)
      descriptivesTable$addRows(row)
    }
  }
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
      
      bainAnalysis <- .bainOneSampleState(variable, options, dataset, bainContainer)
      
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