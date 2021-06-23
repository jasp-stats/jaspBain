context("Bain Linear Regression")

# ==================================================================================================

# library(bain)
# data("sesamesim")
# set.seed(100)
# tt <- lm(age ~ peabody + prenumb + postnumb + funumb, sesamesim)
# bainResult <- bain(tt,"peabody < 0 & prenumb > 0;peabody = 0 & postnumb = 0;postnumb > 0 & funumb > 0",
#                standardize = FALSE, fraction = 1)

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
		list(1, 0.0330714146533409, 0.453860786138191, "H1", 30.2375937189908,
			 1, 13.7236580562284, "H2", 2.20331879409278, 0.0728668694529412,
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
		list(1.26536373845463, 1.16763339997158, 0.0299034902115412, 0.0291567752383641,
			 "H1", 35.3064243610644, 35.3064243610644, 0.904209587796401,
			 0.881630723813583, "H2", 4.00115272103433, 2.57266861476783,
			 0.0658869219920581, 0.0642416708578264, "H3", "", "", "", 0.0249708300902267,
			 "Hu"))
})

test_that("Posterior Probabilities plot matches", {
	skip("Does not reproduce on Linux")
	plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_posteriorProbabilityPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "posterior-probabilities")
})

test_that("Hypothesis Legend table results match", {
	table <- results[["results"]][["legendTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("peabody &lt; 0 &amp; prenumb &gt; 0", "H1", "peabody = 0 &amp; postnumb = 0",
			 "H2", "postnumb &gt; 0 &amp; funumb &gt; 0", "H3"))
})

# ==================================================================================================

# library(bain)
# data("sesamesim")
# set.seed(900)
# tt <- lm(postnumb ~ prenumb + funumb + peabody, sesamesim)
# bainResult <- bain(tt,"prenumb = funumb = peabody = 0;prenumb > 0 & funumb > 0 & peabody > 0 ",
#                standardize = FALSE, fraction = 2)

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
		list(1, 4.77369802413396e-63, "H1", 2.09481201983114e+62, 1, "H2"
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
		list(8.64985149525717e-62, 8.64985149525717e-62, 4.77369802413396e-63,
			 4.52402518195759e-63, "H1", 595.136041110726, 18.1198128820191,
			 1, 0.947698232918355, "H2", "", "", "", 0.052301767081645, "Hu"
			))
})

test_that("Hypothesis Legend table results match", {
	table <- results[["results"]][["legendTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("prenumb = funumb = peabody = 0", "H1", "prenumb &gt; 0 &amp; funumb &gt; 0 &amp; peabody &gt; 0",
			 "H2"))
})

# ==================================================================================================

# library(bain)
# data("sesamesim")
# set.seed(900)
# tt <- lm(postnumb ~ prenumb + funumb + peabody, sesamesim)
# bainResult <- bain(tt,"prenumb = funumb = peabody = 0;prenumb > 0 & funumb > 0 & peabody > 0 ",
#                standardize = TRUE, fraction = 3)

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
		list(1, 2.66173007733809e-164, "H1", 3.75695495390001e+163, 1, "H2"
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
		list(1.05833115528427e-162, 1.05833115528427e-162, 2.66173007733809e-164,
			 2.59642921474827e-164, "H1", 1309.7405575344, 39.7610247671194,
			 1, 0.975466760079922, "H2", "", "", "", 0.0245332399200784,
			 "Hu"))
})

test_that("Hypothesis Legend table results match", {
	table <- results[["results"]][["legendTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("prenumb = funumb = peabody = 0", "H1", "prenumb &gt; 0 &amp; funumb &gt; 0 &amp; peabody &gt; 0",
			 "H2"))
})

# ==================================================================================================