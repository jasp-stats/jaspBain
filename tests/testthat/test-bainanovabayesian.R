context("Bain ANOVA")

# ==================================================================================================

# library(bain)
# data("sesamesim")
# set.seed(100)
# sesamesim$site <- as.factor(sesamesim$site)
# tt <- lm(age ~ site -1, sesamesim)
# bainResult <- bain(tt,"site1 = site2 = site3 = site4 = site5;
#                         site1 < site2 < site3 < site4 < site5;
#                         site1 > site2 > site3 > site4 > site5", fraction = 1)

options <- jaspTools::analysisOptions("BainAnovaBayesian")
options$dependent <- "age"
options$fixedFactors <- "site"
options$bayesFactorMatrix <- TRUE
options$descriptives <- TRUE
options$bayesFactorPlot <- TRUE
options$descriptivesPlot <- TRUE
options$model <- "site1 = site2 = site3 = site4 = site5;site1 < site2 < site3 < site4 < site5;site1 > site2 > site3 > site4 > site5"
set.seed(1)
results <- jaspTools::runAnalysis("BainAnovaBayesian", "sesame.csv", options)


test_that("Bayes Factor Matrix table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 142.769141339179, 8689.31495869546, "H1", 0.00700431473230118,
			 1, 60.8626967787956, "H2", 0.000115083870794589, 0.0164304254153325,
			 1, "H3"))
})

test_that("Descriptives Plot matches", {
	plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesPlots"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "descriptives-plot", dir="BainAnovaBayesian")
})

test_that("Descriptive Statistics table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(60, 48.2744318767126, 49.8666666666667, 7.46169123980563, 0.812379616418173,
			 51.4589014566208, "site1", 55, 49.2096922729294, 50.8727272727273,
			 5.69942994373136, 0.848502836233587, 52.5357622725252, "site2",
			 64, 50.489575293814, 52.03125, 5.6791703787152, 0.786583181296488,
			 53.5729247061861, "site3", 43, 49.5377791422724, 51.4186046511629,
			 5.77446166585718, 0.959622484763091, 53.2994301600533, "site4",
			 18, 47.7596569623255, 50.6666666666667, 7.00420042042, 1.48319547056542,
			 53.5736763710078, "site5"))
})

test_that("Bain ANOVA table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(404.653666701287, 404.653666701287, 0.992930928931594, 0.990500459070911,
			 "H1", 2.87678720723381, 2.83432163915552, 0.00695480073367306,
			 0.00693777695782146, "H2", 0.0461864514395514, 0.0465691102952077,
			 0.000114270334733115, 0.000113990626853698, "H3", "", "", "",
			 0.00244777334441428, "Hu"))
})

test_that("Posterior Probabilities plot matches", {
	skip("Does not reproduce on Linux")
	plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_posteriorProbabilityPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "posterior-probabilities", dir="BainAnovaBayesian")
})

test_that("Hypothesis Legend table results match", {
	table <- results[["results"]][["legendTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("site1 = site2 = site3 = site4 = site5", "H1", "site1 &lt; site2 &lt; site3 &lt; site4 &lt; site5",
			 "H2", "site1 &gt; site2 &gt; site3 &gt; site4 &gt; site5", "H3"
			))
})

# ==================================================================================================

# library(bain)
# data("sesamesim")
# set.seed(900)
# sesamesim$site <- as.factor(sesamesim$site)
# tt <- lm(postnumb ~ site -1, sesamesim)
# bainResult <- bain(tt,"site1 =site2=site3=site4=site5;
#               site3 < site4 < site1 < site5 < site2", fraction = 4)

options <- jaspTools::analysisOptions("BainAnovaBayesian")
options$dependent <- "postnumb"
options$fixedFactors <- "site"
options$bayesFactorMatrix <- TRUE
options$descriptives <- TRUE
options$seed <- 900
options$model <- "site1 = site2 = site3 = site4 = site5;site3 < site4 < site1 < site5 < site2"
set.seed(1)
results <- jaspTools::runAnalysis("BainAnovaBayesian", "sesame.csv", options)


test_that("Bayes Factor Matrix table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 1.63285673189987e-13, "H1", 6124236012038.1, 1, "H2"))
})

test_that("Descriptive Statistics table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(60, 26.8299066659707, 29.6666666666667, 11.4265030412376, 1.44735312642066,
			 32.5034266673626, "site1", 55, 36.018918993238, 38.9818181818182,
			 12.9907244816801, 1.51171103752476, 41.9447173703984, "site2",
			 64, 20.4408189400293, 23.1875, 11.3611353669163, 1.40139363867712,
			 25.9341810599707, "site3", 43, 21.9746619169075, 25.3255813953489,
			 8.94086639626362, 1.70968421097174, 28.6765008737902, "site4",
			 18, 26.5430307468571, 31.7222222222222, 8.511622196638, 2.64249318672075,
			 36.9014136975874, "site5"))
})

test_that("Bain ANOVA table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1.15146657084113e-11, 1.15146657084113e-11, 1.6328567318996e-13,
			 1.61002549190051e-13, "H1", 177.181838391031, 70.5185303980327,
			 0.999999999999837, 0.986017609779649, "H2", "", "", "", 0.0139823902201903,
			 "Hu"))
})

test_that("Hypothesis Legend table results match", {
	table <- results[["results"]][["legendTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("site1 = site2 = site3 = site4 = site5", "H1", "site3 &lt; site4 &lt; site1 &lt; site5 &lt; site2",
			 "H2"))
})

# ==================================================================================================