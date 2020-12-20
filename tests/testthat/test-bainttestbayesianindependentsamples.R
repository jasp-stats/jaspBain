context("Bain Independent Samples T-Test")

options <- jaspTools::analysisOptions("BainTTestBayesianIndependentSamples")
options$variables <- list("age")
options$groupingVariable <- "sex"
options$descriptives <- TRUE
options$bayesFactorPlot <- TRUE
options$descriptivesPlot <- TRUE
options$hypothesis <- "equalBiggerSmaller"
options$bayesFactorType <- "BF01"
set.seed(1)
results <- jaspTools::runAnalysis("BainTTestBayesianIndependentSamples", "sesame.csv", options)


test_that("age plot matches", {
	plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesPlots"]][["collection"]][["bainContainer_descriptivesPlots_age"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "age", dir="BainTTestBayesianIndependentSamples")
})

test_that("Descriptive Statistics table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(115, 1, 50.3127588370044, 51.4260869565217, 6.0914966178275, 0.568034988550375,
			 52.5394150760391, "age", 125, 2, 49.497526347968, 50.632, 6.47144596695559,
			 0.578823723793182, 51.766473652032, ""))
})

test_that("Bain Independent Samples Welch's T-Test table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("", 5.73744721273446, 29.3003302598868, "age", 0.827524901085719,
			 0.144232246572831, 0.0282428523414506, "H0: Equal", "H1: Bigger",
			 "H2: Smaller"))
})

test_that("age plot matches", {
	skip("Does not reproduce on Linux")
	plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_posteriorProbabilityPlot"]][["collection"]][["bainContainer_posteriorProbabilityPlot_age"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "age", dir="BainTTestBayesianIndependentSamples")
})