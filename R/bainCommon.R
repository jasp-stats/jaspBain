##################
### HELPERS ######
##################

# Clean the input for the order constraints
.bainCleanModelInput <- function(input) {
  return(gsub("\n+", ";", input))
}

# Add the Bain citations
.bainGetCitations <- function() {
  citations <- c(
    "Gu, X., Mulder, J., and Hoijtink, H. (2017). Approximate adjusted fractional Bayes factors: A general method for testing informative hypotheses. British Journal of Mathematical and Statistical Psychology, 71, 229-261. DOI: 10.1111/bmsp.12110",
    "Hoijtink, H., Mulder, J., van Lissa, C., and Gu, X. (2018). A Tutorial on testing hypotheses using the Bayes factor. Psychological Methods, 24, 539-556. DOI: 10.1037/met0000201.",
    "Hoijtink, H., Gu, X., and Mulder, J. (2018). Bayesian evaluation of informative hypotheses for multiple populations. British Journal of Mathematical and Statistical Psychology, 72, 219-243. DOI: 10.1111/bmsp.12145"
  )
  return(citations)
}

# Create a container for the results
.bainGetContainer <- function(jaspResults, deps) {
  if (is.null(jaspResults[["bainContainer"]])) {
    jaspResults[["bainContainer"]] <- createJaspContainer()
    jaspResults[["bainContainer"]]$dependOn(options = deps)
    jaspResults[["bainContainer"]]$position = 1
  }
  invisible(jaspResults[["bainContainer"]])
}

# Read the data set
.bainReadDataset <- function(options, type, dataset) {
  
  numerics <- switch(type,
                     "onesampleTTest" = options[["variables"]],
                     "pairedTTest" = unique(unlist(options[["pairs"]])),
                     "independentTTest" = unlist(options[["variables"]]),
                     "anova" = options[["dependent"]],
                     "ancova" = c(options[["dependent"]], unlist(options[["covariates"]])),
                     "regression" = c(options[["dependent"]], unlist(options[["covariates"]])),
                     "sem" = NULL)
  numerics <- numerics[numerics != ""]
  factors <- switch(type,
                    "onesampleTTest" = NULL,
                    "pairedTTest" = NULL,
                    "independentTTest" = options[["groupingVariable"]],
                    "anova" = options[["fixedFactors"]],
                    "ancova" = options[["fixedFactors"]],
                    "regression" = NULL,
                    "sem" = NULL)
  factors <- factors[factors != ""]
  vars <- c(numerics, factors)
  
  if (type != "sem") {
    if (is.null(dataset)) {
      trydata	<- .readDataSetToEnd(columns.as.numeric = numerics, columns.as.factor = factors)
      missing	<- names(which(apply(trydata, 2, function(x) { any(is.na(x))} )))
      if (type == "onesampleTTest") { # For the one sample t test we do not remove the NA's listwise
        dataset	<- .readDataSetToEnd(columns.as.numeric = numerics, columns.as.factor = factors)
      } else {
        dataset	<- .readDataSetToEnd(columns.as.numeric = numerics, columns.as.factor = factors, exclude.na.listwise = vars)
      }
      if ((type == "anova" || type == "ancova") && options[["fixedFactors"]] != "") {
        if (any(grepl(pattern = " ", x = levels(dataset[, options[["fixedFactors"]] ])))) {
          jaspBase:::.quitAnalysis(gettext("Bain does not accept factor levels that contain spaces. Please remove the spaces from your factor levels to continue."))
        }
      }
    } else {
      dataset 	<- .vdf(dataset, columns.as.numeric = numerics, columns.as.factor = factors)
    } 
  } else {
    trydata 	<- .readDataSetToEnd(all.columns = TRUE)
    missing		<- names(which(apply(trydata, 2, function(x) { any(is.na(x))} )))
    dataset		<- .readDataSetToEnd(all.columns = TRUE, exclude.na.listwise = .bainSemGetUsedVars(options[["syntax"]]$model, colnames(trydata)))
  }
  
  readList <- list()
  readList[["dataset"]] <- dataset
  readList[["missing"]] <- missing
  return(readList)
}

##################
### Checks #######
##################

# Check if current options allow for analysis
.bainOptionsReady <- function(options, type, dataset = NULL) {
  ready <- switch(type,
                  "independentTTest" = length(options[["variables"]][options[["variables"]] != ""] > 0) && options[["groupingVariable"]] != "",
                  "pairedTTest" = !is.null(unlist(options[["pairs"]])),
                  "onesampleTTest" = length(options[["variables"]][options[["variables"]] != ""] > 0),
                  "anova" = options[["fixedFactors"]] != "" && options[["dependent"]] != "",
                  "ancova" = options[["dependent"]] != "" && options[["fixedFactors"]] != ""  && !is.null(unlist(options[["covariates"]])),
                  "regression" = (options[["dependent"]] != "" && unlist(options[["covariates"]]) != "" && !is.null(unlist(options[["covariates"]]))),
                  "sem" = length(.bainSemGetUsedVars(options[["syntax"]]$model, colnames(dataset))) > 1)
  return(ready)
}

# Check if current data allow for analysis
.bainDataReady <- function(dataset, options, type) {
  
  if (type == "independentTTest") {
    factors <- options[["groupingVariable"]]
    factors <- factors[factors != ""]
    if (length(factors) > 0)
      .hasErrors(dataset, type = "factorLevels",
                 factorLevels.target = factors, factorLevels.amount = "!= 2",
                 exitAnalysisIfErrors = TRUE)
  }  
  
  numerics <- switch(type,
                     "onesampleTTest" = options[["variables"]],
                     "pairedTTest" = unique(unlist(options[["pairs"]])),
                     "independentTTest" = unlist(options[["variables"]]),
                     "anova" = options[["dependent"]],
                     "ancova" = c(options[["dependent"]], unlist(options[["covariates"]])),
                     "regression" = c(options[["dependent"]], unlist(options[["covariates"]])),
                     "sem" = NULL)
  numerics <- numerics[numerics != ""]
  
  if (length(numerics) > 0)
    .hasErrors(dataset, type = c("infinity", "variance", "observations"),
               all.target = numerics, observations.amount = "< 3",
               exitAnalysisIfErrors = TRUE)
}

############################
### CALLS TO BAIN PACKAGE ##
############################

# Call to bain for 't.test' objects (Welch t-test, Paired t-test, One sample t-test)
.bainTTestRaw <- function(options, x, y = NULL, nu = 0, type = 1, paired = FALSE) {
  
  fraction <- options[["fraction"]]
  
  if (is.null(y) && !paired) {
    x <- x[!is.na(x)] # Here we remove the missing values per dependent variable
    test <- bain::t_test(x = x)
    hypothesis <- switch(type, 
                         "1" = paste0("x=", nu), 
                         "2" = paste0("x=", nu, "; x>", nu), 
                         "3" = paste0("x=", nu, "; x<", nu), 
                         "4" = paste0("x>", nu, "; x<", nu), 
                         "5" = paste0("x=", nu, "; x>", nu, "; x<", nu))
  }
  
  if (!is.null(y) && !paired) {
    test <- bain::t_test(x = x, y = y, paired = FALSE, var.equal = FALSE)
    hypothesis <- switch(type, 
                         "1" = "x=y", 
                         "2" = "x=y; x>y", 
                         "3" = "x=y; x<y", 
                         "4" = "x>y; x<y", 
                         "5" = "x=y; x>y; x<y")
  }
  
  if (!is.null(y) && paired) {
    test <- bain::t_test(x = x, y = y, paired = TRUE)
    hypothesis <- switch(type, 
                         "1" = "difference=0", 
                         "2" = "difference=0;difference>0", 
                         "3" = "difference=0;difference<0", 
                         "4" = "difference>0;difference<0", 
                         "5" = "difference=0;difference>0;difference<0")
  }
  
  bainResult <- bain::bain(x = test, hypothesis = hypothesis, fraction = fraction)
  return(bainResult)
}

# Call to bain for 'lm' objects (ANOVA, ANCOVA, Regression)
.bainRegressionRaw <- function(dataset, options, type) {
  
  dependent <- options[["dependent"]]
  fraction <- options[["fraction"]] # This has to be an object otherwise bain does not like it
  standardized <- options[["standardized"]]
  
  hypothesis <- NULL
  if (options[["model"]] != "")
    hypothesis <- encodeColNames(.bainCleanModelInput(options[["model"]])) 
  
  if (type == "regression") { 
    
    ncov <- length(options[["covariates"]])
    covariates <- paste0(options[["covariates"]], collapse = "+")
    formula <- as.formula(paste0(dependent, "~", covariates))
    
  } else if (type == "anova" || type == "ancova") {
    
    grouping <- options[["fixedFactors"]]
    dataset[, options[["fixedFactors"]]] <- as.factor(dataset[, options[["fixedFactors"]]])
    
    if (type == "anova") {
      formula <- as.formula(paste0(dependent, "~", grouping, "-1"))
    } else {
      ncov <- length(options[["covariates"]])
      covariates <- paste0(options[["covariates"]], collapse = "+")
      formula <- as.formula(paste0(dependent, "~", grouping, "+", covariates, "-1"))
    }
  }
  
  if (type == "regression") {
    # I cannot find out why bain won't work in regression with stats::lm(formula = formula, data = dataset)
    # Error: object of type 'closure' is not subsettable
    args <- list(formula = as.formula(paste0(dependent, "~", covariates)), data = dataset)
    fit <- do.call(stats::lm, args)
  } else {
    fit <- stats::lm(formula = formula, data = dataset)
  }
  
  if (is.null(hypothesis)) {
    if (type == "regression") {
      hypothesis <- paste0(paste0(names(stats::coef(fit))[-1], "=0"), collapse = " & ")
    } else if (type == "anova") {
      hypothesis <- paste0(names(stats::coef(fit)), collapse = "=")		  
    } else if (type == "ancova") {
      hypothesis <- names(stats::coef(fit))
      hypothesis <- hypothesis[1:(length(hypothesis) - ncov)]
      hypothesis <- paste0(hypothesis, collapse = "=")		  
    }
  }
  
  bainResult <- bain::bain(x = fit, hypothesis = hypothesis, fraction = fraction, standardize = standardized)
  return(bainResult)
}

# Call to bain for 'lavaan' objects (SEM)
.bainSemRaw <- function(dataset, options, bainContainer, ready) {
  
  fraction <- options[["fraction"]] # This has to be an object otherwise bain does not like it
  standardized <- options[["standardized"]]
  grouping <- NULL
  if (options[["fixedFactors"]] != "") {
    grouping <- encodeColNames(options[["fixedFactors"]])
    dataset[, grouping] <- as.factor(dataset[, grouping])
  }
  
  if (!is.null(bainContainer[["lavaanResult"]])) {
    
    fit <- bainContainer[["lavaanResult"]]$object
    
  } else if (ready) {
    
    syntax <- .bainSemTranslateModel(options[["syntax"]]$model, dataset)
    
    error <- try({
      fit <- lavaan::sem(model = syntax, data = dataset, group = grouping, std.lv = (options[["factorStandardisation"]] == "std.lv"))
    })
    
    if (isTryError(error)) {
      bainContainer$setError(gettextf("An error occurred in the call to lavaan: %1$s. Please double check if the variables in lavaan syntax match the variables in your data set.", jaspBase::.extractErrorMessage(error)))
      return()
    }
    
    bainContainer[["lavaanResult"]] <- createJaspState(fit)
    bainContainer[["lavaanResult"]]$dependOn(options = "syntax")
  }
  
  if (options[["model"]] == "") {
    # We need to construct a 'default' hypothesis (the results of which will not be shown to the user)
    rhs <- switch(options[["factorStandardisation"]], # If the first loading is standardized the second is taken
                  "std.lv" = fit@ParTable$rhs[1],
                  "auto.fix.first" = fit@ParTable$rhs[2])
    if (options[["fixedFactors"]] != "")
      rhs <- paste0(rhs, ".", levels(dataset[, encodeColNames(options[["fixedFactors"]])])[1])
    hypothesis <- paste0(fit@ParTable$lhs[1], fit@ParTable$op[1], rhs, "= 0")
  } else {
    hypothesis <- encodeColNames(.bainCleanModelInput(options[["model"]]))   
  }
  
  bainResult <- bain::bain(x = fit, hypothesis = hypothesis, fraction = fraction, standardize = standardized)
  return(bainResult)
}

############################################
### EXSTRACTING AND STORING RESULTS ########
############################################

.bainGetGeneralTestResults <- function(dataset, options, bainContainer, ready, type, variable = NULL, pair = NULL, testType = NULL) {
  
  set.seed(options[["seed"]])
  
  if (type %in% c("onesampleTTest", "independentTTest")) {
    result <- .bainGetTTestResults(dataset, options, bainContainer, ready, type, variable, testType)
  } else if (type == "pairedTTest") {
    result <- .bainGetPairedTTestResults(dataset, options, bainContainer, ready, type, pair, testType) 
  } else if (type %in% c("anova", "ancova", "regression", "sem")) {
    result <- .bainGetRegressionResults(dataset, options, bainContainer, ready, type)
  }
  
  return(result) 
}

.bainGetTTestResults <- function(dataset, options, bainContainer, ready, type, variable, testType) {
  
  if (!is.null(bainContainer[[variable]]))
    return(bainContainer[[variable]]$object)
  
  variableData <- dataset[[variable]]
  testValue <- format(options[["testValue"]], scientific = FALSE)  
  
  p <- try({
    if (type == "onesampleTTest") {
      bainResult <- .bainTTestRaw(options, x = variableData, nu = testValue, type = testType)
    } else if (type == "independentTTest") {
      levels <- base::levels(dataset[[options[["groupingVariable"]]]])
      if (length(levels) != 2) {
        g1 <- "1"
        g2 <- "2"
      } else {
        g1 <- levels[1]
        g2 <- levels[2]
      } 
      subDataSet <- dataset[, c(variable, options[["groupingVariable"]])]
      group1 <- subDataSet[subDataSet[[options[["groupingVariable"]]]]== g1, variable]
      group2 <- subDataSet[subDataSet[[options[["groupingVariable"]]]]== g2, variable]
      bainResult <- .bainTTestRaw(options, x = group1, y = group2, type = testType)
    }
  })
  
  if (isTryError(p)) {
    return(p)
  }
  
  bainContainer[[variable]] <- createJaspState(bainResult, dependencies = c("testValue", "hypothesis"))
  bainContainer[[variable]]$dependOn(optionContainsValue = list("variables" = variable))
  return(bainContainer[[variable]]$object)
}

.bainGetPairedTTestResults <- function(dataset, options, bainContainer, ready, type, pair, testType) {
  
  currentPair <- paste(pair, collapse = " - ")
  
  if (!is.null(bainContainer[[currentPair]]))
    return(bainContainer[[currentPair]]$object)
  
  if (pair[[2]] != "" && pair[[1]] != pair[[2]] && pair[[1]] != "") {
    
    subDataSet <- subset(dataset, select=c(pair[[1]], pair[[2]]) )
    c1 <- subDataSet[[ pair[[1]] ]]
    c2 <- subDataSet[[ pair[[2]] ]]  
    
    p <- try({
      bainResult <- .bainTTestRaw(options, x = c1, y = c2, type = testType, paired = TRUE)
    })
    
    if (isTryError(p)) {
      return(p)
    }
    
    bainContainer[[currentPair]] <- createJaspState(bainResult, dependencies = "hypothesis")
    bainContainer[[currentPair]]$dependOn(optionContainsValue = list("pairs" = pair))
    return(bainContainer[[currentPair]]$object) 
  }
}

.bainGetRegressionResults <- function(dataset, options, bainContainer, ready, type) {
  
  if (!is.null(bainContainer[["bainResult"]])) {
    return(bainContainer[["bainResult"]]$object)
    
  } else if (ready) {
    
    if (type == "anova" || type == "ancova") {
      groupCol <- dataset[ , options[["fixedFactors"]]]
      varLevels <- levels(groupCol)
      if (length(varLevels) > 15) {
        bainContainer$setError(gettext("The fixed factor has too many levels for a Bain analysis."))
        return()
      }
    }
    
    p <- try({
      if (type == "sem") {
        bainResult <- .bainSemRaw(dataset, options, bainContainer, ready)
      } else {
        bainResult <- .bainRegressionRaw(dataset, options, type)
      }
    })
    
    if (isTryError(p)) {
      bainContainer$setError(gettextf("An error occurred in the analysis:<br>%1$s<br><br>Please double check if the variables in the 'Model Contraints' section match the variables in your data set.", jaspBase::.extractErrorMessage(p)))
      return()
    }
    
    bainContainer[["bainResult"]] <- createJaspState(bainResult)
    return(bainContainer[["bainResult"]]$object)  
  }
}

.bainExtractTableValuesFromObject <- function(options, bainResult, testType) {
  
  if (testType == 1) {
    BF_0u <- bainResult$fit$BF[1]
    PMP_u <- bainResult$fit$PMPb[2]
    PMP_0 <- bainResult$fit$PMPb[1]
    if (options$bayesFactorType == "BF10")
      BF_0u <- 1/BF_0u
    return(list(BF_0u = BF_0u, PMP_0 = PMP_0, PMP_u = PMP_u))
  } else if (testType == 2) {
    BF_01 <- bainResult$BFmatrix[1,2]
    PMP_1 <- bainResult$fit$PMPa[2]
    PMP_0 <- bainResult$fit$PMPa[1]
    if (options$bayesFactorType == "BF10")
      BF_01 <- 1/BF_01
    return(list(BF_01 = BF_01, PMP_0 = PMP_0, PMP_1 = PMP_1))
  } else if (testType == 3 || testType == 4) {
    BF_01 <- bainResult$BFmatrix[1,2]
    PMP_0 <- bainResult$fit$PMPa[1]
    PMP_1 <- bainResult$fit$PMPa[2]
    if (options$bayesFactorType == "BF10")
      BF_01 <- 1/BF_01
    return(list(BF_01 = BF_01, PMP_0 = PMP_0, PMP_1 = PMP_1))
  } else if (testType == 5) {
    BF_01 <- bainResult$BFmatrix[1,2]
    BF_02 <- bainResult$BFmatrix[1,3]
    BF_12 <- bainResult$BFmatrix[2,3]
    PMP_0 <- bainResult$fit$PMPa[1]
    PMP_1 <- bainResult$fit$PMPa[2]
    PMP_2 <- bainResult$fit$PMPa[3]
    if (options$bayesFactorType == "BF10") {
      BF_01 <- 1/BF_01
      BF_02 <- 1/BF_02
      BF_12 <- 1/BF_12
    }
    return(list(BF_01 = BF_01, BF_02 = BF_02, BF_12 = BF_12, PMP_0 = PMP_0, PMP_1 = PMP_1, PMP_2 = PMP_2))
  }
}

##################
### TABLES #######
##################

# Create a legend containing the order constrained hypotheses
.bainLegend <- function(dataset, options, type, jaspResults, position) {
  
  if (!is.null(jaspResults[["legendTable"]])) 
    return()
  
  legendTable <- createJaspTable("Hypothesis Legend")
  deps <- switch(type, 
                 "regression" = c("model", "covariates"),
                 "anova" = c("model", "fixedFactors"),
                 "ancova" = c("model", "fixedFactors"),
                 "sem" = c("model", "syntax"))
  legendTable$dependOn(options = deps)
  legendTable$position <- position
  legendTable$addColumnInfo(name = "number",     type = "string", title = "")
  legendTable$addColumnInfo(name = "hypothesis", type = "string", title = gettext("Hypothesis"))
  jaspResults[["legendTable"]] <- legendTable
  
  if (options[["model"]] != "") {
    rest.string <- .bainCleanModelInput(options[["model"]])
    hyp.vector <- unlist(strsplit(rest.string, "[;]")) 
    for (i in 1:length(hyp.vector)) {
      row <- list(number = gettextf("H%i", i), hypothesis = hyp.vector[i])
      legendTable$addRows(row)
    }
  } else {
    if (type == "regression") {
      variables <- options[["covariates"]]
      if (length(variables) == 0) {
        row <- list(number = gettext("H1"), hypothesis = "")
      } else if (length(variables) == 1) {
        row <- list(number = gettext("H1"), hypothesis = paste(variables, "= 0"))
      } else {
        row <- list(number = gettext("H1"), hypothesis = paste0(paste0(variables, " = 0"), collapse = " & "))
      }
    } else if (type == "anova" || type == "ancova") {
      if (options[["fixedFactors"]] != "") {
        string <- paste(paste(options[["fixedFactors"]], levels(dataset[, options[["fixedFactors"]]]), sep = ""), collapse = " = ")
        row <- list(number = gettext("H1"), hypothesis = string)
      } else {
        row <- list(number = gettext("H1"), hypothesis = "")
      }
    } else if (type == "sem") {
      row <- list(number = gettext("H1"), hypothesis = "")
    }
    legendTable$addRows(row)
  }
}

# Create a table containing the main analysis results
.bainTestResultsTable <- function(dataset, options, bainContainer, missing, ready, type, position) {
  
  if (!is.null(bainContainer[["mainResultsTable"]])) 
    return()
  
  title <- switch(type,
                  "independentTTest" = gettext("Bain Independent Samples Welch's T-Test"),
                  "pairedTTest" = gettext("Bain Paired Samples T-Test"),
                  "onesampleTTest" = gettext("Bain One Sample T-test"),
                  "anova" = gettext("Bain ANOVA"),
                  "ancova" = gettext("Bain ANCOVA"),
                  "regression" = gettext("Bain Linear Regression"),
                  "sem" = gettext("Bain Structural Equation Model"))
  deps <- switch(type,
                 "independentTTest" = c("variables", "bayesFactorType", "hypothesis"),
                 "pairedTTest" = c("pairs", "hypothesis", "bayesFactorType"),
                 "onesampleTTest" = c("variables", "hypothesis", "bayesFactorType"),
                 "anova" = NULL,
                 "ancova" = NULL,
                 "regression" = "standardized",
                 "sem" = NULL)
  table <- createJaspTable(title)
  table$dependOn(options = deps)
  table$position <- position
  if (type %in% c("independentTTest", "pairedTTest", "onesampleTTest")) {
    bf.type <- options[["bayesFactorType"]]
    BFH1H0 <- FALSE
    if (options$hypothesis == "equalBiggerSmaller") {
      table$addColumnInfo(name="Variable",      type="string",                title="")
      table$addColumnInfo(name="type[equal]",   type="string",                title=gettext("Hypothesis"))
      table$addColumnInfo(name="BF[equal]",     type="number",                title=gettext("BF"))
      table$addColumnInfo(name="pmp[equal]",    type="number", format="dp:3", title=gettext("Posterior probability"))
      table$addColumnInfo(name="type[greater]", type="string",                title=gettext("Hypothesis"))
      table$addColumnInfo(name="BF[greater]",   type="number",                title=gettext("BF"))
      table$addColumnInfo(name="pmp[greater]",  	type="number", format="dp:3", title=gettext("Posterior probability"))
      table$addColumnInfo(name="type[less]",    	type="string",                title=gettext("Hypothesis"))
      table$addColumnInfo(name="BF[less]",      	type="number",                title=gettext("BF"))
      table$addColumnInfo(name="pmp[less]",     	type="number", format="dp:3", title=gettext("Posterior probability"))
    } else {
      table$addColumnInfo(name="Variable",          type="string",                title="")
      table$addColumnInfo(name="hypothesis[type1]", type="string",                title=gettext("Hypothesis"))
      table$addColumnInfo(name="BF[type1]",         type="number",                title=gettext("BF"))
      table$addColumnInfo(name="pmp[type1]",        type="number", format="dp:3", title=gettext("Posterior probability"))
      table$addColumnInfo(name="hypothesis[type2]", type="string",                title=gettext("Hypothesis"))
      table$addColumnInfo(name="BF[type2]",         type="number",                title=gettext("BF"))
      table$addColumnInfo(name="pmp[type2]",        type="number", format="dp:3", title=gettext("Posterior probability"))
    }
    if (type == "onesampleTTest")
      message <- base::switch(options[["hypothesis"]],
                              "equalNotEqual"     = gettextf("The alternative hypothesis H1 specifies that the mean is unequal to %s. The posterior probabilities are based on equal prior probabilities.", options[["testValue"]]),
                              "equalBigger"       = gettextf("The alternative hypothesis H1 specifies that the mean is bigger than %s. The posterior probabilities are based on equal prior probabilities.", options[["testValue"]]),
                              "equalSmaller"      = gettextf("The alternative hypothesis H1 specifies that the mean is smaller than %s. The posterior probabilities are based on equal prior probabilities.", options[["testValue"]]),
                              "biggerSmaller"     = gettextf("The hypothesis H1 specifies that the mean is bigger than %1$s and the hypothesis H2 specifies that the mean is smaller than %1$s. The posterior probabilities are based on equal prior probabilities.", options[["testValue"]]),
                              "equalBiggerSmaller"= gettextf("The null hypothesis H0 with test value %1$s is tested against the other hypotheses. H1 states that the mean is bigger than %1$s and H2 states that the mean is smaller than %1$s. The posterior probabilities are based on equal prior probabilities.", options[["testValue"]]))
    if (type == "independentTTest")
      message <- base::switch(options[["hypothesis"]],
                              "equalNotEqual"     = gettext("The alternative hypothesis H1 specifies that the mean of group 1 is unequal to the mean of group 2. The posterior probabilities are based on equal prior probabilities."),
                              "equalSmaller"      = gettext("The alternative hypothesis H1 specifies that the mean of group 1 is smaller than the mean of group 2. The posterior probabilities are based on equal prior probabilities."),
                              "equalBigger"       = gettext("The alternative hypothesis H1 specifies that mean of group 1 is bigger than the mean of group 2. The posterior probabilities are based on equal prior probabilities."),
                              "biggerSmaller"     = gettext("The hypothesis H1 specifies that the mean of group 1 is bigger than the mean of group 2. The hypothesis H2 specifies that the mean in group 1 is smaller than the mean in group 2. The posterior probabilities are based on equal prior probabilities."),
                              "equalBiggerSmaller"= gettext("The null hypothesis H0 (equal group means) is tested against H1 (first mean larger than second mean) and H2 (first mean smaller than second mean). The posterior probabilities are based on equal prior probabilities."))
    if (type == "pairedTTest")
      message <- base::switch(options[["hypothesis"]],
                              "equalNotEqual"       = gettext("The alternative hypothesis H1 specifies that the mean of variable 1 is unequal to the mean of variable 2. The posterior probabilities are based on equal prior probabilities."),
                              "equalBigger"         = gettext("The alternative hypothesis H1 specifies that the mean of variable 1 is bigger than the mean of variable 2. The posterior probabilities are based on equal prior probabilities."),
                              "equalSmaller"        = gettext("The alternative hypothesis H1 specifies that the mean of variable 1 is smaller than the mean of variable 2. The posterior probabilities are based on equal prior probabilities."),
                              "biggerSmaller"       = gettext("The hypothesis H1 specifies that the mean of variable 1 is bigger than the mean of variable 2, while the hypothesis H2 specifies that it is smaller. The posterior probabilities are based on equal prior probabilities."),
                              "equalBiggerSmaller"  = gettext("The null hypothesis H0 with equal means is tested against the other hypotheses. The alternative hypothesis H1 states that the mean of variable 1 is bigger than the mean of variable 2. The alternative hypothesis H2 states that the mean of variable 1 is smaller than the mean of variable 2. The posterior probabilities are based on equal prior probabilities."))
  } else if (type %in% c("anova", "ancova", "regression", "sem")) {
    table$addColumnInfo(name = "hypotheses", type = "string", title = "")
    table$addColumnInfo(name = "BFu",        type = "number", title = gettext("BF.u"))
    table$addColumnInfo(name = "BF",         type = "number", title = gettext("BF.c"))
    table$addColumnInfo(name = "PMP1",       type = "number", title = gettext("PMP a"))
    table$addColumnInfo(name = "PMP2",       type = "number", title = gettext("PMP b"))
    message <- gettext("BF.u and BF.c denote the Bayes factors of the hypothesis in the row versus \
						the unconstrained hypothesis and complement, respectively. Posterior model probabilities \
						(a: excluding the unconstrained hypothesis, b: including the unconstrained hypothesis) \
						are based on equal prior model probabilities.")
  }
  table$addFootnote(message = message)
  table$addCitation(.bainGetCitations())
  bainContainer[["mainResultsTable"]] <- table
  
  if (!ready || (type == "sem" && options[["model"]] == ""))
    return()
  
  if (type %in% c("onesampleTTest", "independentTTest")) {
    
    testType <- base::switch(options[["hypothesis"]],
                             "equalNotEqual"       = 1,
                             "equalBigger"         = 2,
                             "equalSmaller"        = 3,
                             "biggerSmaller"       = 4,
                             "equalBiggerSmaller"  = 5)
    
    table$setExpectedSize(length(options[["variables"]]))
    startProgressbar(length(options[["variables"]]))
    
    for (variable in options[["variables"]]) {
      
      bainResult <- .bainGetGeneralTestResults(dataset, options, bainContainer, ready, type, variable = variable, testType = testType)
      
      if (isTryError(bainResult)) {
        table$addRows(list(Variable = variable), rowNames = variable)
        table$addFootnote(message = gettextf("Results not computed: %1$s.", jaspBase::.extractErrorMessage(bainResult)), colNames = "Variable", rowNames = variable)
        progressbarTick()
        next
      }
      
      if (variable %in% missing) {
        table$addFootnote(message = gettext("Variable contains missing values, the rows containing these values are removed in the analysis."), colNames = "Variable", rowNames = variable)
      }
      
      tableResults <- .bainExtractTableValuesFromObject(options, bainResult, testType)
      
      if (options$bayesFactorType == "BF01") {
        if (options$hypothesis == "equalNotEqual") {
          row <- list(Variable=variable, "hypothesis[type1]" = gettext("H0: Equal"),"BF[type1]"= tableResults[["BF_0u"]], "pmp[type1]" = tableResults[["PMP_0"]],
                      "hypothesis[type2]" = gettext("H1: Not equal"), "BF[type2]" = "", "pmp[type2]" = tableResults[["PMP_u"]])
        } else if (options$hypothesis == "equalBigger") {
          row <-list(Variable=variable, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]"= tableResults[["BF_01"]], "pmp[type1]" = tableResults[["PMP_0"]],
                     "hypothesis[type2]" = gettext("H1: Bigger"), "BF[type2]" = "", "pmp[type2]" = tableResults[["PMP_1"]])
        } else if (options$hypothesis == "equalSmaller") {
          row <-list(Variable=variable, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]"= tableResults[["BF_01"]], "pmp[type1]" = tableResults[["PMP_0"]],
                     "hypothesis[type2]" = gettext("H1: Smaller"), "BF[type2]" = "", "pmp[type2]" = tableResults[["PMP_1"]])
        } else if (options$hypothesis == "biggerSmaller") {
          row <-list(Variable=variable, "hypothesis[type1]" = gettext("H1: Bigger"), "BF[type1]"= tableResults[["BF_01"]], "pmp[type1]" = tableResults[["PMP_0"]],
                     "hypothesis[type2]" = gettext("H2: Smaller"), "BF[type2]" = "", "pmp[type2]" = tableResults[["PMP_1"]])
        } else if (options$hypothesis == "equalBiggerSmaller") {
          row <-list(Variable=variable, "type[equal]" = gettext("H0: Equal"), "BF[equal]"= "", "pmp[equal]" = tableResults[["PMP_0"]],
                     "type[greater]" =gettext( "H1: Bigger"), "BF[greater]" = tableResults[["BF_01"]], "pmp[greater]" = tableResults[["PMP_1"]],
                     "type[less]" = gettext("H2: Smaller"), "BF[less]" = tableResults[["BF_02"]], "pmp[less]" = tableResults[["PMP_2"]])
        }
      } else if (options$bayesFactorType == "BF10") {
        if (options$hypothesis == "equalNotEqual") {
          row <- list(Variable=variable, "hypothesis[type1]" = gettext("H0: Equal"),"BF[type1]"="", "pmp[type1]" = tableResults[["PMP_0"]],
                      "hypothesis[type2]" = gettext("H1: Not equal"), "BF[type2]" = tableResults[["BF_0u"]], "pmp[type2]" = tableResults[["PMP_u"]])
        } else if (options$hypothesis == "equalBigger") {
          row <-list(Variable=variable, "hypothesis[type1]" = gettext("H0: Equal"),"BF[type1]"="", "pmp[type1]" = tableResults[["PMP_0"]],
                     "hypothesis[type2]" = gettext("H1: Bigger"), "BF[type2]" = tableResults[["BF_01"]], "pmp[type2]" = tableResults[["PMP_1"]])
        } else if (options$hypothesis == "equalSmaller") {
          row <-list(Variable=variable, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]"="", "pmp[type1]" = tableResults[["PMP_0"]],
                     "hypothesis[type2]" = gettext("H1: Smaller"), "BF[type2]" = tableResults[["BF_01"]], "pmp[type2]" = tableResults[["PMP_1"]])
        } else if (options$hypothesis == "biggerSmaller") {
          row <-list(Variable=variable, "hypothesis[type1]" = gettext("H1: Bigger"), "BF[type1]"= "", "pmp[type1]" = tableResults[["PMP_0"]],
                     "hypothesis[type2]" = gettext("H2: Smaller"), "BF[type2]" = tableResults[["BF_01"]], "pmp[type2]" = tableResults[["PMP_1"]])
        } else if (options$hypothesis == "equalBiggerSmaller") {
          row <-list(Variable=variable, "type[equal]" = gettext("H0: Equal"), "BF[equal]"= "", "pmp[equal]" = tableResults[["PMP_0"]],
                     "type[greater]"= gettext("H1: Bigger"), "BF[greater]" = tableResults[["BF_01"]], "pmp[greater]" = tableResults[["PMP_1"]],
                     "type[less]" = gettext("H2: Smaller"), "BF[less]" = tableResults[["BF_02"]], "pmp[less]" = tableResults[["PMP_2"]])
        }
      }
      table$addRows(row, rowNames = variable)
      progressbarTick()
    }
  } else if (type == "pairedTTest") {
    testType <- base::switch(options[["hypothesis"]],
                             "equalNotEqual"       = 1,
                             "equalBigger"         = 2,
                             "equalSmaller"        = 3,
                             "biggerSmaller"       = 4,
                             "equalBiggerSmaller"  = 5)
    
    table$setExpectedSize(length(options[["pairs"]]))
    
    startProgressbar(length(options[["pairs"]]))
    
    for (pair in options[["pairs"]]) {
      
      currentPair <- paste(pair, collapse=" - ")
      
      if (pair[[1]] != "" || pair[[2]] != "") {
        
        bainResult <- .bainGetGeneralTestResults(dataset, options, bainContainer, ready, type, pair = pair, testType = testType)
        
        if (isTryError(bainResult)) {
          table$addRows(list(Variable = currentPair), rowNames = currentPair)
          table$addFootnote(message = gettextf("Results not computed: %s", jaspBase::.extractErrorMessage(bainResult)), colNames = "Variable", rowNames = currentPair)
          progressbarTick()
          next
        } 
        
        if (any(pair %in% missing)) {
          i <- which(pair %in% missing)
          if (length(i) > 1) {
            message <- gettext("Both variables contain missing values, the rows containing these values are removed in the analysis.")
          } else {
            message <- gettextf("The variable %s contains missing values, the rows containing these values are removed in the analysis.", pair[i])
          }
          table$addFootnote(message = message, colNames = "Variable", rowNames = currentPair)
        }
        
        tableResults <- .bainExtractTableValuesFromObject(options, bainResult, testType)
        
        if (options$bayesFactorType == "BF01") {
          if (options$hypothesis == "equalNotEqual") {
            row <- list(Variable=currentPair, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]"= tableResults[["BF_0u"]], "pmp[type1]" = tableResults[["PMP_0"]],
                        "hypothesis[type2]" = gettext("H1: Not equal"), "BF[type2]" = "", "pmp[type2]" = tableResults[["PMP_u"]])
          }
          if (options$hypothesis == "equalSmaller") {
            row <-list(Variable=currentPair, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]"= tableResults[["BF_01"]], "pmp[type1]" = tableResults[["PMP_0"]],
                       "hypothesis[type2]" = gettext("H1: Smaller"), "BF[type2]" = "", "pmp[type2]" = tableResults[["PMP_1"]])
          }
          if (options$hypothesis == "equalBigger") {
            row <-list(Variable=currentPair, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]"= tableResults[["BF_01"]], "pmp[type1]" = tableResults[["PMP_0"]],
                       "hypothesis[type2]" = gettext("H1: Bigger"), "BF[type2]" = "", "pmp[type2]" = tableResults[["PMP_1"]])
          }
          if (options$hypothesis == "biggerSmaller") {
            row <-list(Variable=currentPair, "hypothesis[type1]" = gettext("H1: Bigger"), "BF[type1]"= tableResults[["BF_01"]], "pmp[type1]" = tableResults[["PMP_0"]],
                       "hypothesis[type2]" = gettext("H2: Smaller"), "BF[type2]" = "", "pmp[type2]" = tableResults[["PMP_1"]])
          }
          if (options$hypothesis == "equalBiggerSmaller") {
            row <-list(Variable=currentPair,
                       "type[equal]" = gettext("H0: Equal"),
                       "BF[equal]"= "",
                       "pmp[equal]" = tableResults[["PMP_0"]],
                       "type[greater]"= gettext("H1: Bigger"),
                       "BF[greater]" = tableResults[["BF_01"]],
                       "pmp[greater]" = tableResults[["PMP_1"]],
                       "type[less]" = gettext("H2: Smaller"),
                       "BF[less]" = tableResults[["BF_02"]],
                       "pmp[less]" = tableResults[["PMP_2"]])
          }
        } else if (options$bayesFactorType == "BF10") {
          if (options$hypothesis == "equalNotEqual") {
            row <- list(Variable=currentPair, "hypothesis[type1]" = gettext("H0: Equal"),"BF[type1]"="", "pmp[type1]" = tableResults[["PMP_0"]],
                        "hypothesis[type2]" = gettext("H1: Not equal"), "BF[type2]" = tableResults[["BF_0u"]], "pmp[type2]" = tableResults[["PMP_u"]])
          }
          if (options$hypothesis == "equalSmaller") {
            row <-list(Variable=currentPair, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]"= "", "pmp[type1]" = tableResults[["PMP_0"]],
                       "hypothesis[type2]" = gettext("H1: Smaller"), "BF[type2]" = tableResults[["BF_01"]], "pmp[type2]" = tableResults[["PMP_1"]])
          }
          if (options$hypothesis == "equalBigger") {
            row <-list(Variable=currentPair, "hypothesis[type1]" = gettext("H0: Equal"), "BF[type1]"= "", "pmp[type1]" = tableResults[["PMP_0"]],
                       "hypothesis[type2]" = gettext("H1: Bigger"), "BF[type2]" = tableResults[["BF_01"]], "pmp[type2]" = tableResults[["PMP_1"]])
          }
          if (options$hypothesis == "biggerSmaller") {
            row <-list(Variable=currentPair, "hypothesis[type1]" = gettext("H1: Bigger"), "BF[type1]"= "", "pmp[type1]" = tableResults[["PMP_0"]],
                       "hypothesis[type2]" = gettext("H2: Smaller"), "BF[type2]" = tableResults[["BF_01"]], "pmp[type2]" = tableResults[["PMP_1"]])
          }
          if (options$hypothesis == "equalBiggerSmaller") {
            row <-list(Variable=currentPair,
                       "type[equal]" = gettext("H0: Equal"),
                       "BF[equal]"= "",
                       "pmp[equal]" = tableResults[["PMP_0"]],
                       "type[greater]"= gettext("H1: Bigger"),
                       "BF[greater]" = tableResults[["BF_01"]],
                       "pmp[greater]" = tableResults[["PMP_1"]],
                       "type[less]" = gettext("H2: Smaller"),
                       "BF[less]" = tableResults[["BF_02"]],
                       "pmp[less]" = tableResults[["PMP_2"]])
          }
        }
      } else {
        if (options$hypothesis == "equalBiggerSmaller") {
          row <- list(Variable=currentPair, "type[equal]" = ".", "BF[equal]"= ".", "pmp[equal]" = ".",
                      "type[greater]"= ".", "BF[greater]" = ".", "pmp[greater]" = ".",
                      "type[less]" = ".", "BF[less]" = ".", "pmp[less]" = ".")
        } else {
          row <- list(Variable=currentPair, "hypothesis[type1]" = ".", "BF[type1]"= ".", "pmp[type1]" = ".",
                      "hypothesis[type2]" = ".", "BF[type2]" = ".", "pmp[type2]" = ".")
        }
      }
      table$addRows(row, rowNames = currentPair)
      
      if (pair[[1]] == pair[[2]]) {
        table$addFootnote(message=gettext("Results not computed: The variables in this pair are the same."), colNames="Variable", rowNames=currentPair)
      }
      if (pair[[1]] == "" || pair[[2]] == "") {
        table$addFootnote(message=gettext("Results not computed: The pair is incomplete."), colNames="Variable", rowNames=currentPair)
      }
      progressbarTick()
    }
  } else if (type %in% c("anova", "ancova", "regression", "sem")) {
    if (bainContainer$getError())
      return()
    variables <- switch(type,
                        "anova" = c(options[["dependent"]], options[["fixedFactors"]]),
                        "ancova" = c(options[["dependent"]], options[["fixedFactors"]], unlist(options[["covariates"]])),
                        "regression" = c(options[["dependent"]], unlist(options[["covariates"]])),
                        "sem" = .bainSemGetUsedVars(.bainSemTranslateModel(options[["syntax"]]$model, dataset), colnames(dataset)))
    if (any(variables %in% missing)) {
      i <- which(variables %in% missing)
      if (length(i) > 1) {
        if (type == "regression" || type == "ancova" || type == "sem") {
          table$addFootnote(message = gettextf("The variables %1$s contain missing values, the rows containing these values are removed in the analysis.", paste(variables[i], collapse = ", ")), symbol=gettext("<b>Warning.</b>"))
        } else if (type == "anova") {
          table$addFootnote(message= gettextf("The variables %1$s and %2$s contain missing values, the rows containing these values are removed in the analysis.", variables[1], variables[2]), symbol=gettext("<b>Warning.</b>"))
        }
      } else if (length(i) == 1) {
        table$addFootnote(message = gettextf("The variable %1$s contains missing values, the rows containing these values are removed in the analysis.", variables[i]), symbol=gettext("<b>Warning.</b>"))
      }
    }
    bainResult <- .bainGetGeneralTestResults(dataset, options, bainContainer, ready, type)  
    for (i in 1:(length(bainResult[["fit"]]$BF)-1)) {
      row <- list(hypotheses = gettextf("H%1$i", i), BFu = bainResult[["fit"]]$BF.u[i], BF = bainResult[["fit"]]$BF.c[i], PMP1 = bainResult[["fit"]]$PMPa[i], PMP2 = bainResult[["fit"]]$PMPb[i])
      table$addRows(row)
    }
    row <- list(hypotheses = gettext("Hu"), BFu = "", BF = "", PMP1 = "", PMP2 = bainResult[["fit"]]$PMPb[length(bainResult[["fit"]]$BF)])
    table$addRows(row) 
  }
  
  if(any(is.nan(unlist(bainResult[["fit"]]))))
    table$addFootnote(message = gettext("<b>Warning:</b> The entered model contraints are incompatible with the data and therefore the computed results contain NaN's."))
}

# Create the Bayes factor matrix
.bainBfMatrix <- function(dataset, options, bainContainer, ready, type, position) {
  
  if (!is.null(bainContainer[["bayesFactorMatrix"]]) || !options[["bayesFactorMatrix"]]) 
    return() 
  
  bayesFactorMatrix <- createJaspTable(gettext("Bayes Factor Matrix"))
  bayesFactorMatrix$position <- position
  bayesFactorMatrix$dependOn(options = c("bayesFactorMatrix", "standardized")) 
  bayesFactorMatrix$addColumnInfo(name = "hypothesis",  title = "",             type = "string")
  bayesFactorMatrix$addColumnInfo(name = "H1",          title = gettext("H1"),  type = "number")
  bainContainer[["bayesFactorMatrix"]] <- bayesFactorMatrix
  
  if (!ready || bainContainer$getError() || (type == "sem" && options[["model"]] == "")) {
    row <- data.frame(hypothesis = gettext("H1"), H1 = ".")
    bayesFactorMatrix$addRows(row)
    return()
  }
  
  bainResult <- .bainGetGeneralTestResults(dataset, options, bainContainer, ready, type)
  BFmatrix <- bainResult[["BFmatrix"]]
  if (nrow(BFmatrix) > 1) {
    for (i in 2:nrow(BFmatrix))
      bayesFactorMatrix$addColumnInfo(name = paste0("H", i), title = gettextf("H%i", i), type = "number")
  }
  for (i in 1:nrow(BFmatrix)) {
    tmp <- list(hypothesis = gettextf("H%i", i))
    for (j in 1:ncol(BFmatrix)) {
      tmp[[paste0("H", j)]] <- BFmatrix[i,j]
    }
    row <- tmp
    bayesFactorMatrix$addRows(row)
  }
}

# Create the descriptive statistics table
.bainDescriptivesTable <- function(dataset, options, bainContainer, ready, type, position) {
  
  if (!is.null(bainContainer[["descriptivesTable"]]) || !options[["descriptives"]]) 
    return()
  
  if (type == "ancova") {
    title <- gettext("Coefficients for Groups plus Covariates")
  } else if (type == "regression" || type == "sem") {
    title <- gettext("Coefficients for Parameters")
  } else {
    title <- gettext("Descriptive Statistics")
  }
  meanTitle <- ifelse(type %in% c("ancova", "regression"), yes = gettext("Coefficient"), no = gettext("Mean"))
  table <- createJaspTable(title)
  table$dependOn(options = c("variables", "descriptives", "credibleInterval", "pairs"))
  table$position <- position
  overTitle <- gettextf("%.0f%% Credible Interval", 100 * options[["credibleInterval"]])
  table$addColumnInfo(name = "v",                title = "",                 type = "string")
  if (type == "independentTTest")
    table$addColumnInfo(name = "group",          title = gettext("Group"),   type = "string")
  table$addColumnInfo(name = "N",                title = gettext("N"),       type = "integer")
  table$addColumnInfo(name = "mean",             title = meanTitle,          type = "number")
  if (type %in% c("independentTTest", "pairedTTest", "onesampleTTest", "anova"))
    table$addColumnInfo(name = "sd",             title = gettext("SD"),      type = "number")
  table$addColumnInfo(name = "se",               title = gettext("SE"),      type = "number")
  table$addColumnInfo(name = "lowerCI",          title = gettext("Lower"),   type = "number", overtitle = overTitle)
  table$addColumnInfo(name = "upperCI",          title = gettext("Upper"),   type = "number", overtitle = overTitle)
  bainContainer[["descriptivesTable"]] <- table
  if (type == "regression" || type == "sem")
    table$addFootnote(message = ifelse(options[["standardized"]], yes = gettext("The displayed coefficients are standardized."), no = gettext("The displayed coefficients are unstandardized.")))
  
  if (!ready || bainContainer$getError())
    return()
  
  if (type == "independentTTest") {
    table$setExpectedSize(length(options[["variables"]]) * 2)
    levels <- base::levels(dataset[[ options[["groupingVariable"]] ]])
    if (length(levels) != 2) {
      g1 <- "1"
      g2 <- "2"
    } else {
      g1 <- levels[1]
      g2 <- levels[2]
    }
    for (variable in options[["variables"]]) {
      bainResult <- .bainGetGeneralTestResults(dataset, options, bainContainer, ready, type, variable = variable) 
      if (isTryError(bainResult)) { 
        table$addRows(list(v = variable), rowNames = variable)
        table$addFootnote(message = gettext("The results for this variable were not computed."), colNames = "v", rowNames = variable)
      } else {
        bainSummary <- summary(bainResult, ci = options[["credibleInterval"]])
        N <- bainSummary[["n"]]
        mu <- bainSummary[["Estimate"]]
        CiLower <- bainSummary[["lb"]]
        CiUpper <- bainSummary[["ub"]]
        sd <- aggregate(dataset[, variable], list(dataset[, options[["groupingVariable"]]]), sd)[, 2]
        se <- sqrt(diag(bainResult[["posterior"]]))
        row <- data.frame(v = variable, group = g1, N = N[1], mean = mu[1], sd = sd[1], se = se[1], lowerCI = CiLower[1], upperCI = CiUpper[1])
        table$addRows(row)
        row <- data.frame(v = "", group = g2, N = N[2], mean = mu[2], sd = sd[2], se = se[2], lowerCI = CiLower[2], upperCI = CiUpper[2])
        table$addRows(row) 
      }
    }
  } else if (type == "pairedTTest") {
    table$setExpectedSize(length(options[["pairs"]]))
    for (pair in options[["pairs"]]) {
      if (pair[[2]] != "" && pair[[1]] != pair[[2]]) {
        subDataSet <- subset(dataset, select=c(pair[[1]], pair[[2]]))
        c1 <- subDataSet[[ pair[[1]] ]]
        c2 <- subDataSet[[ pair[[2]] ]]
        difference <- c1 - c2
        currentPair <- paste(pair, collapse=" - ")
        testType <- base::switch(options[["hypothesis"]],
                                 "equalNotEqual"       = 1,
                                 "equalBigger"         = 2,
                                 "equalSmaller"        = 3,
                                 "biggerSmaller"       = 4,
                                 "equalBiggerSmaller"  = 5)
        bainResult <- .bainGetGeneralTestResults(dataset, options, bainContainer, ready, type, pair = pair, testType = testType)
        if (isTryError(bainResult)) {
          table$addRows(list(v = currentPair), rowNames = currentPair)
          table$addFootnote(message = gettext("The results for this variable were not computed."), colNames = "v", rowNames = currentPair)
        } else {     
          bainSummary <- summary(bainResult, ci = options[["credibleInterval"]])
          N <- bainSummary[["n"]]
          mu <- bainSummary[["Estimate"]]
          CiLower <- bainSummary[["lb"]]
          CiUpper <- bainSummary[["ub"]]
          se <- sqrt(diag(bainResult[["posterior"]]))
          sd <- sd(difference, na.rm = TRUE)   
          row <- list(v = currentPair, N = N, mean = mu, sd = sd, se = se, lowerCI = CiLower, upperCI = CiUpper)
          table$addRows(row)
        }
      }
    }
  } else if (type == "onesampleTTest") {
    table$setExpectedSize(length(options[["variables"]]))
    for (variable in options[["variables"]]) {
      bainResult <- .bainGetGeneralTestResults(dataset, options, bainContainer, ready, type, variable = variable)
      if (isTryError(bainResult)) {
        table$addRows(list(v = variable), rowNames = variable)
        table$addFootnote(message = gettext("The results for this variable were not computed."), colNames = "v", rowNames = variable) 
      } else {   
        bainSummary <- summary(bainResult, ci = options[["credibleInterval"]])   
        N <- bainSummary[["n"]]
        mu <- bainSummary[["Estimate"]]
        CiLower <- bainSummary[["lb"]]
        CiUpper <- bainSummary[["ub"]]
        sd <- sd(dataset[, variable], na.rm = TRUE)
        se <- sqrt(diag(bainResult[["posterior"]]))  
        row <- list(v = variable, N = N, mean = mu, sd = sd, se = se, lowerCI = CiLower, upperCI = CiUpper)
        table$addRows(row)
      }
    }
  } else if (type %in% c("anova", "ancova")) {
    groupCol <- dataset[ , options[["fixedFactors"]]]
    varLevels <- levels(groupCol)
    bainResult <- .bainGetGeneralTestResults(dataset, options, bainContainer, ready, type)
    bainSummary <- summary(bainResult, ci = options[["credibleInterval"]])
    sigma <- diag(bainResult[["posterior"]])
    variable <- bainSummary[["Parameter"]]
    N        <- bainSummary[["n"]]
    mu       <- bainSummary[["Estimate"]]
    CiLower  <- bainSummary[["lb"]]
    CiUpper  <- bainSummary[["ub"]]
    se <- sqrt(sigma)	
    row <- data.frame(v = variable, N = N, mean = mu, se = se, lowerCI = CiLower, upperCI = CiUpper)
    if (type == "anova") {
      sd <- aggregate(dataset[, options[["dependent"]]], list(dataset[, options[["fixedFactors"]]]), sd)[, 2]
      row <- cbind(row, sd = sd)
    }
    table$addRows(row)
  } else if (type %in% c("regression", "sem")) {
    bainResult <- .bainGetGeneralTestResults(dataset, options, bainContainer, ready, type)
    if (is.null(bainResult))
      return()
    bainSummary <- summary(bainResult, ci = options[["credibleInterval"]])
    groups <- bainSummary[["Parameter"]]
    N <- bainSummary[["n"]]
    mu <- bainSummary[["Estimate"]]
    CiLower <- bainSummary[["lb"]]
    CiUpper <- bainSummary[["ub"]]
    se <- sqrt(diag(bainResult[["posterior"]]))
    row <- data.frame(v = groups, N = N, mean = mu, se = se, lowerCI = CiLower, upperCI = CiUpper)
    table$addRows(row)
  }
}

##################
### PLOTS ########
##################

# Create the posterior probability plots
.bainPosteriorProbabilityPlot <- function(dataset, options, bainContainer, ready, type, position) {
  
  if (!is.null(bainContainer[["posteriorProbabilityPlot"]]) || !options[["bayesFactorPlot"]]) 
    return()
  
  if (type %in% c("independentTTest", "pairedTTest", "onesampleTTest")) {
    container <- createJaspContainer(gettext("Posterior Probabilities"))
    container$dependOn(options = c("variables", "bayesFactorPlot", "hypothesis", "pairs"))
    container$position <- position
    bainContainer[["posteriorProbabilityPlot"]] <- container
    if (!ready || bainContainer$getError())
      return()
    analysisType <- base::switch(options[["hypothesis"]],
                                 "equalNotEqual" = 1,
                                 "equalBigger" = 2,
                                 "equalSmaller"	= 3,
                                 "biggerSmaller" = 4,
                                 "equalBiggerSmaller" = 5)
    if (type == "onesampleTTest" || type == "independentTTest") {  
      for(variable in options[["variables"]]) {     
        if (is.null(container[[variable]])) {       
          bainResult <- .bainGetGeneralTestResults(dataset, options, bainContainer, ready, type, variable = variable)
          plot <- createJaspPlot(plot = NULL, title = variable, height = 300, width = 400)
          plot$dependOn(optionContainsValue = list("variables" = variable))
          if (isTryError(bainResult) || any(is.nan(unlist(bainResult[["fit"]])))) {
            plot$setError(gettext("Plotting not possible: the results for this variable were not computed."))
          } else {
            p <- try({
              plot$plotObject <- .plotModelProbabilitiesTTests(bainResult, type = analysisType)
            })
            if (isTryError(p)) {
              plot$setError(gettextf("Plotting not possible: %1$s", jaspBase::.extractErrorMessage(p)))
            }
          }    
          container[[variable]] <- plot
        }
      } 
    } else if (type == "pairedTTest") {
      for(pair in options[["pairs"]]) {   
        currentPair <- paste(pair, collapse=" - ")
        if (is.null(container[[currentPair]]) && pair[[2]] != "" && pair[[1]] != pair[[2]]) {
          bainResult <- .bainGetGeneralTestResults(dataset, options, bainContainer, ready, type, pair = pair)
          plot <- createJaspPlot(plot = NULL, title = currentPair, height = 300, width = 400)
          plot$dependOn(optionContainsValue = list("pairs" = pair))
          if (isTryError(bainResult) || any(is.nan(unlist(bainResult[["fit"]])))) {
            plot$setError(gettext("Plotting not possible: the results for this variable were not computed."))
          } else {
            p <- try({
              plot$plotObject <- .plotModelProbabilitiesTTests(bainResult, type = analysisType)
            })
            if (isTryError(p)) {
              plot$setError(gettextf("Plotting not possible: %1$s", jaspBase::.extractErrorMessage(p)))
            }
          }    
          container[[currentPair]] <- plot
        }
      }
    }
  } else if (type %in% c("anova", "ancova", "regression", "sem")) {
    if (options[["model"]] == "") {
      height <- 300
      width <- 400
    } else {
      height <- 400
      width <- 600
    }
    plot <- createJaspPlot(plot = NULL, title = gettext("Posterior Probabilities"), height = height, width = width)
    plot$dependOn(options = c("bayesFactorPlot", "standardized"))
    plot$position <- position
    bainContainer[["posteriorProbabilityPlot"]] <- plot
    if (!ready || bainContainer$getError()  || (type == "sem" && options[["model"]] == ""))
      return()
    bainResult <- .bainGetGeneralTestResults(dataset, options, bainContainer, ready, type)
    if (is.null(bainResult) || any(is.nan(unlist(bainResult[["fit"]])))) {
      plot$setError(gettext("Plotting not possible: the results could not be computed."))
    } else {
      p <- try({
        plot$plotObject <- .suppressGrDevice(.plotModelProbabilitiesRegression(bainResult))
      })
      if (isTryError(p)) {
        plot$setError(gettextf("Plotting not possible: %1$s", jaspBase::.extractErrorMessage(p)))
      }
    }   
  }
}

# Create the descriptive plot(s)
.bainDescriptivePlots <- function(dataset, options, bainContainer, ready, type, position) {
  
  if (!is.null(bainContainer[["descriptivesPlots"]]) || !options[["descriptivesPlot"]]) 
    return()
  
  if (type %in% c("independentTTest", "pairedTTest", "onesampleTTest")) {
    container <- createJaspContainer(gettext("Descriptive Plots"))
    container$dependOn(options = c("variables", "pairs", "descriptivesPlot", "credibleInterval"))
    container$position <- position
    bainContainer[["descriptivesPlots"]] <- container
  } else {
    plot <- createJaspPlot(plot = NULL, title = ifelse(type == "anova", yes = gettext("Descriptives Plot"), no = gettext("Adjusted Means")))
    plot$dependOn(options = c("descriptivesPlot", "credibleInterval"))
    plot$position <- position
    bainContainer[["descriptivesPlots"]] <- plot
  }
  
  if (!ready || bainContainer$getError() || (type == "sem" && options[["model"]] == ""))
    return()
  
  if (type %in% c("independentTTest", "onesampleTTest")) {
    
    for (variable in options[["variables"]]) {
      
      if (is.null(bainContainer[["descriptivesPlots"]][[variable]])) {  
        
        bainResult <- .bainGetGeneralTestResults(dataset, options, bainContainer, ready, type, variable = variable)
        if (isTryError(bainResult)) {
          container[[variable]] <- createJaspPlot(plot=NULL, title = variable)
          container[[variable]]$dependOn(optionContainsValue=list("variables" = variable))
          container[[variable]]$setError(gettext("Plotting not possible: the results for this variable were not computed."))
          
        } else {
          
          if (type == "onesampleTTest") {
            
            bainSummary <- summary(bainResult, ci = options[["credibleInterval"]])
            N <- bainSummary[["n"]]
            mu <- bainSummary[["Estimate"]]
            CiLower <- bainSummary[["lb"]]
            CiUpper <- bainSummary[["ub"]]
            yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(options[["testValue"]], CiLower, CiUpper), min.n = 4)
            plotData <- data.frame(v = variable, N = N, mean = mu, lowerCI = CiLower, upperCI = CiUpper, index = 1)
            p <- ggplot2::ggplot(plotData, ggplot2::aes(x = index, y = mean)) +
              ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = Inf, y = options[["testValue"]], yend = options[["testValue"]]), linetype = 2, color = "darkgray") +
              ggplot2::geom_errorbar(ggplot2::aes(ymin = lowerCI, ymax = upperCI), colour="black", width = .1, position = ggplot2::position_dodge(.2)) +
              ggplot2::geom_point(position = ggplot2::position_dodge(.2), size = 4) +
              ggplot2::ylab("") +
              ggplot2::xlab("") +
              ggplot2::scale_y_continuous(breaks = yBreaks, labels = yBreaks, limits = range(yBreaks)) +
              ggplot2::scale_x_continuous(breaks = 0:2, labels = NULL)
            p <- jaspGraphs::themeJasp(p, sides = "l") + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
            
          } else if (type == "independentTTest") {
            
            bainSummary <- summary(bainResult, ci = options[["credibleInterval"]])
            levels <- base::levels(dataset[[ options[["groupingVariable"]] ]])
            N <- bainSummary[["n"]]
            mu <- bainSummary[["Estimate"]]
            CiLower <- bainSummary[["lb"]]
            CiUpper <- bainSummary[["ub"]]
            yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(CiLower, CiUpper), min.n = 4)
            plotData <- data.frame(v = levels, N = N, mean = mu, lowerCI = CiLower, upperCI = CiUpper, index = 1:length(levels))
            p <- ggplot2::ggplot(plotData, ggplot2::aes(x = index, y = mean)) +
              ggplot2::geom_errorbar(ggplot2::aes(ymin = lowerCI, ymax = upperCI), colour = "black", width = .2, position = ggplot2::position_dodge(.2)) +
              ggplot2::geom_line(position = ggplot2::position_dodge(.2), size = .7) +
              ggplot2::geom_point(position = ggplot2::position_dodge(.2), size = 4) +
              ggplot2::ylab(variable) +
              ggplot2::xlab(options[["groupingVariable"]]) +
              ggplot2::scale_x_continuous(breaks = 1:length(levels), labels = as.character(levels)) +
              ggplot2::scale_y_continuous(breaks = yBreaks, labels = yBreaks, limits = range(yBreaks))
            p <- jaspGraphs::themeJasp(p)
          }
          container[[variable]] <- createJaspPlot(plot = p, title = variable)
          container[[variable]]$dependOn(optionContainsValue = list("variables" = variable))
        }
      }
    }
    
  } else if (type == "pairedTTest") {
    
    for (pair in options[["pairs"]]) {
      currentPair <- paste(pair, collapse =" - ")
      
      if (is.null(bainContainer[["descriptivesPlots"]][[currentPair]]) && pair[[2]] != "" && pair[[1]] != pair[[2]]) {   
        bainResult <- .bainGetGeneralTestResults(dataset, options, bainContainer, ready, type, pair = pair)     
        if (isTryError(bainResult)) { 
          container[[currentPair]] <- createJaspPlot(plot = NULL, title = currentPair)
          container[[currentPair]]$dependOn(optionContainsValue = list("variables" = currentPair))
          container[[currentPair]]$setError(gettext("Plotting not possible: the results for this variable were not computed."))
          
        } else {
          
          bainSummary <- summary(bainResult, ci = options[["credibleInterval"]])
          N <- bainSummary[["n"]]
          mu <- bainSummary[["Estimate"]]
          CiLower <- bainSummary[["lb"]]
          CiUpper <- bainSummary[["ub"]]
          yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, CiLower, CiUpper), min.n = 4)
          plotData <- data.frame(v = gettext("Difference"), N = N, mean = mu, lowerCI = CiLower, upperCI = CiUpper, index = 1)
          p <- ggplot2::ggplot(plotData, ggplot2::aes(x=index, y=mean)) +
            ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = Inf, y = 0, yend = 0), linetype = 2, color = "darkgray") +
            ggplot2::geom_errorbar(ggplot2::aes(ymin=lowerCI, ymax=upperCI), colour="black", width=.1, position = ggplot2::position_dodge(.2)) +
            ggplot2::geom_point(position=ggplot2::position_dodge(.2), size=4) +
            ggplot2::ylab("") +
            ggplot2::xlab("") +
            ggplot2::scale_y_continuous(breaks = yBreaks, labels = yBreaks, limits = range(yBreaks)) +
            ggplot2::scale_x_continuous(breaks = 0:2, labels = NULL)
          p <- jaspGraphs::themeJasp(p, sides = "l") + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())  
          container[[currentPair]] <- createJaspPlot(plot = p, title = currentPair)
          container[[currentPair]]$dependOn(optionContainsValue = list("pairs" = pair))
        }
      }
    }
    
  } else if (type %in% c("anova", "ancova")) {
    
    groupCol <- dataset[ , options[["fixedFactors"]]]
    varLevels <- levels(groupCol)
    bainResult <- bainContainer[["bainResult"]]$object
    bainSummary <- summary(bainResult, ci = options[["credibleInterval"]])
    
    if (type == "ancova")
      bainSummary <- bainSummary[1:length(varLevels), ]
    
    variable <- bainSummary[["Parameter"]]
    N <- bainSummary[["n"]]
    mu <- bainSummary[["Estimate"]]
    CiLower <- bainSummary[["lb"]]
    CiUpper <- bainSummary[["ub"]]
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(pretty(c(CiLower, CiUpper)), min.n = 4)
    plotData <- data.frame(v = variable, N = N, mean = mu, lowerCI = CiLower, upperCI = CiUpper, index = 1:length(variable))
    p <- ggplot2::ggplot(plotData, ggplot2::aes(x=index, y=mean)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin=lowerCI, ymax=upperCI), colour="black", width=.2, position = ggplot2::position_dodge(.2)) +
      ggplot2::geom_line(position=ggplot2::position_dodge(.2), size = .7) +
      ggplot2::geom_point(position=ggplot2::position_dodge(.2), size=4) +
      ggplot2::scale_fill_manual(values = c(rep(c("white","black"),5),rep("grey",100)), guide=ggplot2::guide_legend(nrow=10)) +
      ggplot2::scale_shape_manual(values = c(rep(c(21:25),each=2),21:25,7:14,33:112), guide=ggplot2::guide_legend(nrow=10)) +
      ggplot2::scale_color_manual(values = rep("black",200), guide=ggplot2::guide_legend(nrow=10)) +
      ggplot2::scale_y_continuous(name = options[["dependent"]], breaks = yBreaks) +
      ggplot2::scale_x_continuous(name = options[["fixedFactors"]], breaks = 1:length(varLevels), labels = as.character(varLevels))
    p <- jaspGraphs::themeJasp(p)
    plot$plotObject <- p
  }
}

.plotModelProbabilitiesTTests <- function(x, type) {
  
  if (type == 1 || type == 2 || type == 3) {
    labs <- c(gettext("H0"), gettext("H1"))
  } else if (type == 4) {
    labs <- c(gettext("H1"), gettext("H2"))
  } else if (type == 5) {
    labs <- c(gettext("H0"), gettext("H1"), gettext("H2"))
  }
  labels <- rev(labs)
  
  if (type == 1) {
    values <- x$fit$PMPb
  } else {
    values <- na.omit(x$fit$PMPa)
  }
  
  ggdata <- data.frame(lab = labs, PMP = values)
  
  p <- ggplot2::ggplot(data = ggdata, mapping = ggplot2::aes(x = "", y = PMP, fill = lab)) +
    ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
    ggplot2::geom_col() +
    ggplot2::coord_polar(theta = "y", direction = -1) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "none") +
    ggplot2::scale_y_continuous(breaks = cumsum(rev(values)) - rev(values)/2, labels = labels) +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   axis.text=ggplot2::element_text(size=17, color = "black"),
                   plot.title = ggplot2::element_text(size=18, hjust = .5),
                   axis.ticks.y = ggplot2::element_blank()) +
    ggplot2::scale_fill_brewer(palette="Set1")
  
  return(p)
}

.plotModelProbabilitiesRegression <- function(x) {
  
  PMPa <- na.omit(x$fit$PMPa)
  PMPb <- x$fit$PMPb
  numH <- length(PMPa)
  P_lables <- paste(gettext("H"), 1:numH, sep = "")
  ggdata1 <- data.frame(lab = P_lables, PMP = PMPa)
  ggdata2 <- data.frame(lab = c(P_lables, gettext("Hu")), PMP = PMPb)
  
  if (numH == 1) {
    
    p <- ggplot2::ggplot(data = ggdata2, mapping = ggplot2::aes(x = "", y = PMP, fill = lab)) +
      ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
      ggplot2::geom_col() + 
      ggplot2::coord_polar(theta = "y", direction = -1) +
      ggplot2::labs(x = "", y = "", title = "") +
      ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "none") +
      ggplot2::scale_y_continuous(breaks = cumsum(rev(PMPb)) - rev(PMPb)/2, labels = rev(c(P_lables, gettext("Hu")))) +
      ggplot2::theme(panel.background = ggplot2::element_blank(),
                     axis.text=ggplot2::element_text(size=17, color = "black"),
                     plot.title = ggplot2::element_text(size=18, hjust = .5),
                     axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::scale_fill_brewer(palette="Set1")
    
    return(p)
    
  } else if (numH > 1) {
    
    p1 <- ggplot2::ggplot(data = ggdata1, mapping = ggplot2::aes(x = "", y = PMP, fill = lab)) +
      ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
      ggplot2::geom_col() +
      ggplot2::coord_polar(theta = "y", direction = -1) +
      ggplot2::labs(x = "", y = "", title = gettext("Excluding Hu"), size = 30) +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),legend.position = "none") +
      ggplot2::scale_y_continuous(breaks = cumsum(rev(PMPa)) - rev(PMPa)/2, labels = rev(P_lables)) +            
      ggplot2::theme(panel.background = ggplot2::element_blank(),
                     axis.text=ggplot2::element_text(size=17, color = "black"),
                     plot.title = ggplot2::element_text(size=18, hjust = .5),
                     axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::scale_fill_brewer(palette="Set1")
    
    p2 <- ggplot2::ggplot(data = ggdata2, mapping = ggplot2::aes(x = "", y = PMP, fill = lab)) +
      ggplot2::geom_bar(stat = "identity", width = 1e10, color = "black", size = 1) +
      ggplot2::geom_col() + 
      ggplot2::coord_polar(theta = "y", direction = -1) +
      ggplot2::labs(x = "", y = "", title = gettext("Including Hu"), size = 30) +
      ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "none") +
      ggplot2::scale_y_continuous(breaks = cumsum(rev(PMPb)) - rev(PMPb)/2, labels = rev(c(P_lables, gettext("Hu")))) +
      ggplot2::theme(panel.background = ggplot2::element_blank(),
                     axis.text=ggplot2::element_text(size=17, color = "black"),
                     plot.title = ggplot2::element_text(size=18, hjust = .5),
                     axis.ticks.y = ggplot2::element_blank()) +
      ggplot2::scale_fill_brewer(palette="Set1")
    
    plotMat <- list(p1 = p1, p2 = p2)
    pp <- jaspGraphs::ggMatrixPlot(plotList = plotMat, layout = matrix(c(1, 2), ncol = 2))
    
    return(pp)
  }
}