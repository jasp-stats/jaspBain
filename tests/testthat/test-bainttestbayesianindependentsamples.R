context("Bain Independent Samples T-Test")

# ==================================================================================================

# library(bain)
# data("sesamesim")
# x <-sesamesim$age[which(sesamesim$sex==1)]
# y <-sesamesim$age[which(sesamesim$sex==2)]
# set.seed(100)
# tt <- t_test(x,y,paired = FALSE, var.equal = FALSE)
# bainResult <- bain(tt,"x=y;x>y;x<y", fraction = 1)

options <- jaspTools::analysisOptions("BainTTestBayesianIndependentSamples")
options$variables <- list("age")
options$groupingVariable <- "sex"
options$descriptives <- TRUE
options$bayesFactorPlot <- TRUE
options$descriptivesPlot <- TRUE
options$hypothesis <- "equalBiggerSmaller"
options$bayesFactorType <- "BF01"
options$seed <- 100
options$fraction <- 1
set.seed(1)
results <- jaspTools::runAnalysis("BainTTestBayesianIndependentSamples", "sesame.csv", options)


test_that("age plot matches", {
	plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesPlots"]][["collection"]][["bainContainer_descriptivesPlots_age"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "age")
})

test_that("Descriptive Statistics table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(115, 1, 50.3127588370044, 51.4260869565217, 6.0914966178275, 0.568034988550375,
			 52.5394150760391, "age", 125, 2, 49.497526347968, 50.632, 6.47144596695559,
			 0.578823723793183, 51.766473652032, ""))
})

test_that("Bain Independent Samples Welch's T-Test table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("", 5.73744721273447, 29.3003302598868, "age", 0.827524901085719,
			 0.144232246572831, 0.0282428523414506, "H0: Equal", "H1: Bigger",
			 "H2: Smaller"))
})

test_that("age plot matches", {
	plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_posteriorProbabilityPlot"]][["collection"]][["bainContainer_posteriorProbabilityPlot_age"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "age-2")
})

# ==================================================================================================

# library(bain)
# data("sesamesim")
# x <-sesamesim$postnumb[which(sesamesim$sex==1)]
# y <-sesamesim$postnumb[which(sesamesim$sex==2)]
# set.seed(900)
# tt <- t_test(x,y,paired = FALSE, var.equal = FALSE)
# bainResult <- bain(tt,"x=y",fraction = 4)

options <- jaspTools::analysisOptions("BainTTestBayesianIndependentSamples")
options$variables <- list("postnumb")
options$groupingVariable <- "sex"
options$seed <- 900
options$fraction <- 4
options$descriptives <- TRUE
options$hypothesis <- "equalNotEqual"
options$bayesFactorType <- "BF01"
set.seed(1)
results <- jaspTools::runAnalysis("BainTTestBayesianIndependentSamples", "sesame.csv", options)


test_that("Descriptive Statistics table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(115, 1, 27.709094405675, 30.095652173913, 13.0578832229393, 1.21765388908315,
			 32.4822099421511, "postnumb", 125, 2, 26.7239441460813, 28.856,
			 12.1620138400333, 1.08780358758431, 30.9880558539187, ""))
})

test_that("Bain Independent Samples Welch's T-Test table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(5.79280615163151, "", "postnumb", "H0: Equal", "H1: Not equal",
			 0.852785435403627, 0.147214564596373))
})

# ==================================================================================================

# library(bain)
# data("sesamesim")
# x <-sesamesim$postnumb[which(sesamesim$sex==1)]
# y <-sesamesim$postnumb[which(sesamesim$sex==2)]
# set.seed(900)
# tt <- t_test(x,y,paired = FALSE, var.equal = FALSE)
# bainResult <- bain(tt,"x=y;x>y", fraction = 4)

test_that("Bain Independent Samples Welch's T-Test table results match", {
	options <- jaspTools::analysisOptions("BainTTestBayesianIndependentSamples")
	options$variables <- list("postnumb")
	options$groupingVariable <- "sex"
	options$seed <- 900
	options$fraction <- 4
	options$hypothesis <- "equalBigger"
	options$bayesFactorType <- "BF01"
	set.seed(1)
	results <- jaspTools::runAnalysis("BainTTestBayesianIndependentSamples", "sesame.csv", options)
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(3.73180139745868, "", "postnumb", "H0: Equal", "H1: Bigger", 0.788663995801457,
			 0.211336004198543))
})

# ==================================================================================================

# library(bain)
# data("sesamesim")
# x <-sesamesim$postnumb[which(sesamesim$sex==1)]
# y <-sesamesim$postnumb[which(sesamesim$sex==2)]
# set.seed(900)
# tt <- t_test(x,y,paired = FALSE, var.equal = FALSE)
# bainResult <- bain(tt,"x=y;x<y", fraction = 4)

test_that("Bain Independent Samples Welch's T-Test table results match", {
	options <- jaspTools::analysisOptions("BainTTestBayesianIndependentSamples")
	options$variables <- list("postnumb")
	options$groupingVariable <- "sex"
	options$seed <- 900
	options$fraction <- 4
	options$hypothesis <- "equalSmaller"
	options$bayesFactorType <- "BF01"
	set.seed(1)
	results <- jaspTools::runAnalysis("BainTTestBayesianIndependentSamples", "sesame.csv", options)
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(12.9384998340372, "", "postnumb", "H0: Equal", "H1: Smaller",
			 0.928256267754293, 0.0717437322457074))
})

# ==================================================================================================

# library(bain)
# data("sesamesim")
# x <-sesamesim$postnumb[which(sesamesim$sex==1)]
# y <-sesamesim$postnumb[which(sesamesim$sex==2)]
# set.seed(900)
# tt <- t_test(x,y,paired = FALSE, var.equal = FALSE)
# bainResult <- bain(tt,"x>y;x<y", fraction = 4)

test_that("Bain Independent Samples Welch's T-Test table results match", {
	options <- jaspTools::analysisOptions("BainTTestBayesianIndependentSamples")
	options$variables <- list("postnumb")
	options$groupingVariable <- "sex"
	options$seed <- 900
	options$fraction <- 4
	options$hypothesis <- "biggerSmaller"
	options$bayesFactorType <- "BF01"
	set.seed(1)
	results <- jaspTools::runAnalysis("BainTTestBayesianIndependentSamples", "sesame.csv", options)
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(3.46709228493461, "", "postnumb", "H1: Bigger", "H2: Smaller",
			 0.77614073401337, 0.22385926598663))
})

# ==================================================================================================

# library(bain)
# data("sesamesim")
# x <-sesamesim$postnumb[which(sesamesim$sex==1)]
# y <-sesamesim$postnumb[which(sesamesim$sex==2)]
# set.seed(900)
# tt <- t_test(x,y,paired = FALSE, var.equal = FALSE)
# bainResult <- bain(tt,"x=y;x>y;x<y", fraction = 4)

test_that("Bain Independent Samples Welch's T-Test table results match", {
	options <- jaspTools::analysisOptions("BainTTestBayesianIndependentSamples")
	options$variables <- list("postnumb")
	options$groupingVariable <- "sex"
	options$seed <- 900
	options$fraction <- 4
	options$hypothesis <- "equalBiggerSmaller"
	options$bayesFactorType <- "BF01"
	set.seed(1)
	results <- jaspTools::runAnalysis("BainTTestBayesianIndependentSamples", "sesame.csv", options)
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("", 3.73180139745868, 12.9384998340372, "postnumb", 0.743353041114553,
			 0.19919415905165, 0.057452799833797, "H0: Equal", "H1: Bigger",
			 "H2: Smaller"))
})

# ==================================================================================================