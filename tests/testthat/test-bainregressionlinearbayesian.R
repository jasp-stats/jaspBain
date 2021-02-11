context("Bain Linear Regression")

# ==================================================================================================

options <- jaspTools::analysisOptions("BainRegressionLinearBayesian")
options$dependent <- "age"
options$covariates <- list("peabody", "prenumb", "postnumb", "funumb")
options$bayesFactorMatrix <- TRUE
options$descriptives <- TRUE
options$bayesFactorPlot <- TRUE
options$model <- "peabody < 0 & prenumb > 0;peabody = 0 & postnumb = 0;postnumb > 0 & funumb > 0"
set.seed(1)
results <- jaspTools::runAnalysis("BainRegressionLinearBayesian", "sesame.csv", options)


test_that("Bayes Factor Matrix table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 0.0336654885723126, 0.471498933944235, "H1", 29.7040097265193,
			 1, 14.0054089199231, "H2", 2.12089556944422, 0.0714009855561927,
			 1, "H3"))
})

test_that("Coefficients for Parameters table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(240, -0.0486909406866819, 0.00982367365032013, 0.0298549436614947,
			 0.0683382879873222, "peabody", 240, 0.0633098838945105, 0.167649321189282,
			 0.0532353849957385, 0.271988758484053, "prenumb", 240, -0.046805449165961,
			 0.0420375226466933, 0.045328879771994, 0.130880494459348, "postnumb",
			 240, -0.0355734195087462, 0.00669477754047436, 0.0215658029344553,
			 0.048962974589695, "funumb"))
})

test_that("Bain Linear Regression table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1.29950533181896, 1.18860802585663, 0.0304646728142417, 0.0297033594892114,
			 "H1", 35.3064243610644, 35.3064243610644, 0.904922937589465,
			 0.882308879177836, "H2", 3.88021871697742, 2.52091349584517,
			 0.0646123895962929, 0.0629977235382773, "H3", "", "", "", 0.0249900377946751,
			 "Hu"))
})

test_that("Posterior Probabilities plot matches", {
	skip("Does not reproduce on Linux")
	plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_posteriorProbabilityPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "posterior-probabilities", dir="BainRegressionLinearBayesian")
})

test_that("Hypothesis Legend table results match", {
	table <- results[["results"]][["legendTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("peabody &lt; 0 &amp; prenumb &gt; 0", "H1", "peabody = 0 &amp; postnumb = 0",
			 "H2", "postnumb &gt; 0 &amp; funumb &gt; 0", "H3"))
})

# ==================================================================================================

options <- jaspTools::analysisOptions("BainRegressionLinearBayesian")
options$dependent <- "postnumb"
options$covariates <- list("prenumb", "funumb", "peabody")
options$bayesFactorMatrix <- TRUE
options$descriptives <- TRUE
options$seed <- 900
options$fraction <- 2
options$model <- "prenumb = funumb = peabody = 0;prenumb > 0 & funumb > 0 & peabody > 0"
set.seed(1)
results <- jaspTools::runAnalysis("BainRegressionLinearBayesian", "sesame.csv", options)


test_that("Bayes Factor Matrix table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 4.89942014782784e-63, "H1", 2.04105785955783e+62, 1, "H2"
			))
})

test_that("Coefficients for Parameters table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(240, 0.494098974883347, 0.621254756578271, 0.0648765909465239,
			 0.748410538273195, "prenumb", 240, 0.131783988339985, 0.187566866610198,
			 0.0284611751594524, 0.24334974488041, "funumb", 240, -0.00151905887489465,
			 0.0818592041766284, 0.0425407118238907, 0.165237467228151, "peabody"
			))
})

test_that("Bain Linear Regression table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(8.64985149525717e-62, 8.64985149525717e-62, 4.89942014782784e-63,
			 4.6367849169709e-63, "H1", 615.089712026728, 17.6548473784027,
			 1, 0.946394629786265, "H2", "", "", "", 0.053605370213735, "Hu"
			))
})

test_that("Hypothesis Legend table results match", {
	table <- results[["results"]][["legendTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("prenumb = funumb = peabody = 0", "H1", "prenumb &gt; 0 &amp; funumb &gt; 0 &amp; peabody &gt; 0",
			 "H2"))
})

# ==================================================================================================

options <- jaspTools::analysisOptions("BainRegressionLinearBayesian")
options$dependent <- "postnumb"
options$covariates <- list("prenumb", "funumb", "peabody")
options$bayesFactorMatrix <- TRUE
options$descriptives <- TRUE
options$seed <- 900
options$fraction <- 3
options$standardized <- TRUE
options$model <- "prenumb = funumb = peabody = 0;prenumb > 0 & funumb > 0 & peabody > 0"
set.seed(1)
results <- jaspTools::runAnalysis("BainRegressionLinearBayesian", "sesame.csv", options)


test_that("Bayes Factor Matrix table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 2.78911650280699e-164, "H1", 3.58536475257878e+163, 1, "H2"
			))
})

test_that("Coefficients for Parameters table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(240, 0.425136718359189, 0.523997033619579, 0.0504398632016645,
			 0.622857348879968, "prenumb", 240, 0.214535343365428, 0.30273821649852,
			 0.0450022928119215, 0.390941089631612, "funumb", 240, -0.000932877627412801,
			 0.104533362133665, 0.0538102947773436, 0.209999601894742, "peabody"
			))
})

test_that("Bain Linear Regression table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1.05833115528427e-162, 1.05833115528427e-162, 2.78911650280699e-164,
			 2.71749975620954e-164, "H1", 1360.87407219717, 37.9450322071219,
			 1, 0.974322784105513, "H2", "", "", "", 0.0256772158944865,
			 "Hu"))
})

test_that("Hypothesis Legend table results match", {
	table <- results[["results"]][["legendTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("prenumb = funumb = peabody = 0", "H1", "prenumb &gt; 0 &amp; funumb &gt; 0 &amp; peabody &gt; 0",
			 "H2"))
})

# ==================================================================================================