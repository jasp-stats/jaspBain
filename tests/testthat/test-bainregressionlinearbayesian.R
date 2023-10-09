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
		list(1, 0.0328810427741221, 0.461267814187202, "H1", 30.4126607805461,
			 1, 14.0283815618593, "H2", 2.16793795110568, 0.0712840605019485,
			 1, "H3"))
})

test_that("Coefficients for Parameters table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(240, -0.0486909406866819, 0.0098236736503201, 0.0298549436614947,
			 0.0683382879873221, "peabody", 240, 0.0633098838945105, 0.167649321189282,
			 0.0532353849957385, 0.271988758484053, "prenumb", 240, -0.046805449165961,
			 0.0420375226466934, 0.045328879771994, 0.130880494459348, "postnumb",
			 240, -0.0355734195087463, 0.00669477754047435, 0.0215658029344553,
			 0.048962974589695, "funumb"))
})

test_that("Bain Linear Regression table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1.25568016554222, 1.16091204961746, 0.0297790997710067, 0.0290343266334664,
			 0.0292945416680495, "H1", 35.3064243610644, 35.3064243610644,
			 0.905661659685665, 0.883011126895188, 0.890924958471963, "H2",
			 3.8843338673906, 2.51678529026158, 0.064559240543328, 0.0629446185934903,
			 0.0635087486424113, "H3", "", "", "", 0.0250099278778557, "",
			 "Hu", "", 0.644832483502042, "", "", 0.0162717512175767, "Hc"
			))
})

test_that("Posterior Probabilities plot matches", {
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
		list(1, 4.93057122691304e-63, "H1", 2.02816256773982e+62, 1, "H2"
			))
})

test_that("Coefficients for Parameters table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(240, 0.494098974883348, 0.621254756578272, 0.0648765909465239,
			 0.748410538273196, "prenumb", 240, 0.131783988339985, 0.187566866610198,
			 0.0284611751594524, 0.24334974488041, "funumb", 240, -0.0015190588748946,
			 0.0818592041766284, 0.0425407118238907, 0.165237467228151, "peabody"
			))
})

test_that("Bain Linear Regression table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(8.64985149525571e-62, 8.64985149525571e-62, 4.93057122691304e-63,
			 4.6646762733537e-63, 4.92503532253631e-63, "H1", 889.653250377439,
			 17.5433050191859, 1, 0.946072180824004, 0.998877228596453, "H2",
			 "", "", "", 0.0539278191759961, "", "Hu", "", 0.0197192614220688,
			 "", "", 0.00112277140354703, "Hc"))
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
		list(1, 2.68421368638488e-164, "H1", 3.72548581013611e+163, 1, "H2"
			))
})

test_that("Coefficients for Parameters table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(240, 0.425136718359189, 0.523997033619579, 0.0504398632016646,
			 0.622857348879969, "prenumb", 240, 0.214535343365429, 0.302738216498521,
			 0.0450022928119215, 0.390941089631612, "funumb", 240, -0.000932877627413217,
			 0.104533362133664, 0.0538102947773436, 0.209999601894741, "peabody"
			))
})

test_that("Bain Linear Regression table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1.05833115528577e-162, 1.05833115528577e-162, 2.68421368638488e-164,
			 2.61781873208312e-164, 2.68314544905412e-164, "H1", 2511.75031222721,
			 39.427977014421, 1, 0.975264653988418, 0.999602029698242, "H2",
			 "", "", "", 0.0247353460115823, "", "Hu", "", 0.0156974110135412,
			 "", "", 0.00039797030175817, "Hc"))
})

test_that("Hypothesis Legend table results match", {
	table <- results[["results"]][["legendTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("prenumb = funumb = peabody = 0", "H1", "prenumb &gt; 0 &amp; funumb &gt; 0 &amp; peabody &gt; 0",
			 "H2"))
})

# ==================================================================================================