context("Bain One Sample T-Test")

# ==================================================================================================

options <- jaspTools::analysisOptions("BainTTestBayesianOneSample")
options$variables <- list("age")
options$testValue <- 51
options$descriptives <- TRUE
options$bayesFactorPlot <- TRUE
options$descriptivesPlot <- TRUE
options$hypothesis <- "equalBiggerSmaller"
options$bayesFactorType <- "BF01"
set.seed(1)
results <- jaspTools::runAnalysis("BainTTestBayesianOneSample", "sesame.csv", options)


test_that("age plot matches", {
	plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesPlots"]][["collection"]][["bainContainer_descriptivesPlots_age"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "age", dir="BainTTestBayesianOneSample")
})

test_that("Descriptive Statistics table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(240, 50.2165034595134, 51.0125, 6.29171019316781, 0.406128146621706,
			 51.8084965404866, "age"))
})

test_that("Bain One Sample T-test table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("", 15.1135035681881, 15.8743731825382, "age", 0.885613607789322,
			 0.0585975054555463, 0.0557888867551317, "H0: Equal", "H1: Bigger",
			 "H2: Smaller"))
})

test_that("age plot matches", {
	skip("Does not reproduce on Linux")
	plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_posteriorProbabilityPlot"]][["collection"]][["bainContainer_posteriorProbabilityPlot_age"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "age", dir="BainTTestBayesianOneSample")
})

# ==================================================================================================

test_that("Bain One Sample T-test table results match", {
	options <- jaspTools::analysisOptions("BainTTestBayesianOneSample")
	options$variables <- list("postnumb")
	options$testValue <- 30
	options$fraction <- 4
	options$hypothesis <- "equalNotEqual"
	options$bayesFactorType <- "BF01"
	set.seed(1)
	results <- jaspTools::runAnalysis("BainTTestBayesianOneSample", "sesame.csv", options)
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(6.16007603449514, "", "postnumb", "H0: Equal", "H1: Not equal",
			 0.860336678663426, 0.139663321336574))
})

# ==================================================================================================

test_that("Bain One Sample T-test table results match", {
	options <- jaspTools::analysisOptions("BainTTestBayesianOneSample")
	options$variables <- list("postnumb")
	options$testValue <- 30
	options$fraction <- 4
	options$hypothesis <- "equalBigger"
	options$bayesFactorType <- "BF01"
	set.seed(1)
	results <- jaspTools::runAnalysis("BainTTestBayesianOneSample", "sesame.csv", options)
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(12.3576651279191, "", "postnumb", "H0: Equal", "H1: Bigger", 0.925136617034224,
			 0.0748633829657758))
})

# ==================================================================================================

test_that("Bain One Sample T-test table results match", {
	options <- jaspTools::analysisOptions("BainTTestBayesianOneSample")
	options$variables <- list("postnumb")
	options$testValue <- 30
	options$fraction <- 4
	options$hypothesis <- "equalSmaller"
	options$bayesFactorType <- "BF01"
	set.seed(1)
	results <- jaspTools::runAnalysis("BainTTestBayesianOneSample", "sesame.csv", options)
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(4.10256609199388, "", "postnumb", "H0: Equal", "H1: Smaller",
			 0.804020176912743, 0.195979823087257))
})

# ==================================================================================================

test_that("Bain One Sample T-test table results match", {
	options <- jaspTools::analysisOptions("BainTTestBayesianOneSample")
	options$variables <- list("postnumb")
	options$testValue <- 30
	options$fraction <- 4
	options$hypothesis <- "biggerSmaller"
	options$bayesFactorType <- "BF01"
	set.seed(1)
	results <- jaspTools::runAnalysis("BainTTestBayesianOneSample", "sesame.csv", options)
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.331985536873396, "", "postnumb", "H1: Bigger", "H2: Smaller",
			 0.249241097356563, 0.750758902643437))
})

# ==================================================================================================

test_that("Bain One Sample T-test table results match", {
	options <- jaspTools::analysisOptions("BainTTestBayesianOneSample")
	options$variables <- list("postnumb")
	options$testValue <- 30
	options$fraction <- 4
	options$hypothesis <- "equalBiggerSmaller"
	options$bayesFactorType <- "BF01"
	set.seed(1)
	results <- jaspTools::runAnalysis("BainTTestBayesianOneSample", "sesame.csv", options)
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("", 12.3576651279191, 4.10256609199388, "postnumb", 0.754904244574999,
			 0.0610879350395631, 0.184007820385438, "H0: Equal", "H1: Bigger",
			 "H2: Smaller"))
})

# ==================================================================================================