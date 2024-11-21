context("Bain Paired Samples T-Test")

# ==================================================================================================

# library(bain)
# data("sesamesim")
# set.seed(100)
# tt <- t_test(sesamesim$prenumb,sesamesim$postnumb,paired = TRUE)
# bainAnalysis <- bain(tt,"difference=0;difference>0;difference<0", fraction = 1)

options <- jaspTools::analysisOptions("BainTTestBayesianPairedSamples")
options$descriptives <- TRUE
options$bayesFactorPlot <- TRUE
options$descriptivesPlot <- TRUE
options$hypothesis <- "equalBiggerSmaller"
options$bayesFactorType <- "BF01"
options$seed <- 100
options$fraction <- 1
options$pairs <- list(list("prenumb", "postnumb"))
set.seed(1)
results <- jaspTools::runAnalysis("BainTTestBayesianPairedSamples", "sesame.csv", options)


test_that("prenumb - postnumb plot matches", {
	plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesPlots"]][["collection"]][["bainContainer_descriptivesPlots_prenumb - postnumb"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "prenumb-postnumb")
})

test_that("Descriptive Statistics table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(240, -9.89599389735516, -8.69166666666667, 9.51923473009165, 0.61446395963807,
			 -7.48733943597818, "prenumb - postnumb"))
})

test_that("Bain Paired Samples T-Test table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("", 276.004579526015, 2.76244529575384e-43, "prenumb - postnumb",
			 2.76244529575384e-43, 1.00086936981184e-45, 1, "H0: Equal",
			 "H1: Bigger", "H2: Smaller"))
})

test_that("prenumb - postnumb plot matches", {
	plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_posteriorProbabilityPlot"]][["collection"]][["bainContainer_posteriorProbabilityPlot_prenumb - postnumb"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "prenumb-postnumb-2")
})

# ==================================================================================================

# library(bain)
# data("sesamesim")
# set.seed(900)
# tt <- t_test(sesamesim$postnumb,sesamesim$prenumb,paired = TRUE)
# bainAnalysis <- bain(tt,"difference=0", fraction = 4)

test_that("Bain Paired Samples T-Test table results match", {
	options <- jaspTools::analysisOptions("BainTTestBayesianPairedSamples")
	options$seed <- 900
	options$fraction <- 4
	options$hypothesis <- "equalNotEqual"
	options$bayesFactorType <- "BF01"
	options$pairs <- list(list("postnumb", "prenumb"))
	set.seed(1)
	results <- jaspTools::runAnalysis("BainTTestBayesianPairedSamples", "sesame.csv", options)
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(2.76244529575384e-43, "", "postnumb - prenumb", "H0: Equal", "H1: Not equal",
			 2.76244529575384e-43, 1))
})

# ==================================================================================================

# library(bain)
# data("sesamesim")
# set.seed(900)
# tt <- t_test(sesamesim$postnumb,sesamesim$prenumb,paired = TRUE)
# bainAnalysis <- bain(tt,"difference=0;difference>0", fraction = 4)

test_that("Bain Paired Samples T-Test table results match", {
	options <- jaspTools::analysisOptions("BainTTestBayesianPairedSamples")
	options$seed <- 900
	options$fraction <- 4
	options$hypothesis <- "equalBigger"
	options$bayesFactorType <- "BF01"
	options$pairs <- list(list("postnumb", "prenumb"))
	set.seed(1)
	results <- jaspTools::runAnalysis("BainTTestBayesianPairedSamples", "sesame.csv", options)
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1.38122264787692e-43, "", "postnumb - prenumb", "H0: Equal", "H1: Bigger",
			 1.38122264787692e-43, 1))
})

# ==================================================================================================

# library(bain)
# data("sesamesim")
# set.seed(900)
# tt <- t_test(sesamesim$postnumb,sesamesim$prenumb,paired = TRUE)
# bainAnalysis <- bain(tt,"difference=0;difference<0", fraction = 4)

test_that("Bain Paired Samples T-Test table results match", {
	options <- jaspTools::analysisOptions("BainTTestBayesianPairedSamples")
	options$seed <- 900
	options$fraction <- 4
	options$hypothesis <- "equalSmaller"
	options$bayesFactorType <- "BF01"
	options$pairs <- list(list("postnumb", "prenumb"))
	set.seed(1)
	results <- jaspTools::runAnalysis("BainTTestBayesianPairedSamples", "sesame.csv", options)
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(138.002289763008, "", "postnumb - prenumb", "H0: Equal", "H1: Smaller",
			 0.992805873905351, 0.0071941260946489))
})

# ==================================================================================================

# library(bain)
# data("sesamesim")
# set.seed(900)
# tt <- t_test(sesamesim$postnumb,sesamesim$prenumb,paired = TRUE)
# bainAnalysis <- bain(tt,"difference>0;difference<0", fraction = 4)

test_that("Bain Paired Samples T-Test table results match", {
	options <- jaspTools::analysisOptions("BainTTestBayesianPairedSamples")
	options$seed <- 900
	options$fraction <- 4
	options$hypothesis <- "biggerSmaller"
	options$bayesFactorType <- "BF01"
	options$pairs <- list(list("postnumb", "prenumb"))
	set.seed(1)
	results <- jaspTools::runAnalysis("BainTTestBayesianPairedSamples", "sesame.csv", options)
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(9.99131385335528e+44, "", "postnumb - prenumb", "H1: Bigger",
			 "H2: Smaller", 1, 1.00086936981184e-45))
})

# ==================================================================================================

# library(bain)
# data("sesamesim")
# set.seed(900)
# tt <- t_test(sesamesim$postnumb,sesamesim$prenumb,paired = TRUE)
# bainAnalysis <- bain(tt,"difference=0;difference>0;difference<0", fraction = 4)

test_that("Bain Paired Samples T-Test table results match", {
	options <- jaspTools::analysisOptions("BainTTestBayesianPairedSamples")
	options$seed <- 900
	options$fraction <- 4
	options$hypothesis <- "equalBiggerSmaller"
	options$bayesFactorType <- "BF01"
	options$pairs <- list(list("postnumb", "prenumb"))
	set.seed(1)
	results <- jaspTools::runAnalysis("BainTTestBayesianPairedSamples", "sesame.csv", options)
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("", 1.38122264787692e-43, 138.002289763008, "postnumb - prenumb",
			 1.38122264787692e-43, 1, 1.00086936981184e-45, "H0: Equal",
			 "H1: Bigger", "H2: Smaller"))
})

# ==================================================================================================