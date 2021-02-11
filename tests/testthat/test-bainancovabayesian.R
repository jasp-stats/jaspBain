context("Bain ANCOVA")

# ==================================================================================================

options <- jaspTools::analysisOptions("BainAncovaBayesian")
options$dependent <- "age"
options$fixedFactors <- "site"
options$covariates <- list("peabody", "prenumb", "postnumb", "funumb")
options$bayesFactorMatrix <- TRUE
options$descriptives <- TRUE
options$bayesFactorPlot <- TRUE
options$descriptivesPlot <- TRUE
options$model <- "site1 = site2 = site3 = site4 = site5;site1 < site2 < site3 < site4 < site5;site1 > site2 > site3 > site4 > site5"
set.seed(1)
results <- jaspTools::runAnalysis("BainAncovaBayesian", "sesame.csv", options)


test_that("Bayes Factor Matrix table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 0.0125382587162236, 25240.6918749615, "H1", 79.7558913588277,
			 1, 2013093.87900107, "H2", 3.96185653291061e-05, 4.96747822061937e-07,
			 1, "H3"))
})

test_that("Adjusted Means plot matches", {
	plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesPlots"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "adjusted-means", dir="BainAncovaBayesian")
})

test_that("Coefficients for Groups plus Covariates table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(60, 47.996998358907, 49.470232843929, 0.7516640594637, 50.9434673289511,
			 "site1", 55, 46.0602043737956, 47.8562155128899, 0.91634905195247,
			 49.6522266519841, "site2", 64, 52.9297945198661, 54.4143247835134,
			 0.757427317724733, 55.8988550471606, "site3", 43, 50.7941602448356,
			 52.4831836355637, 0.861762463010012, 54.1722070262919, "site4",
			 18, 47.6044724127278, 50.1889163156235, 1.31861805792427, 52.7733602185193,
			 "site5", 240, 0.00400716421086952, 0.0687595426462479, 0.0330375348456078,
			 0.133511921081626, "peabody", 240, 0.0810760411118373, 0.185400315885238,
			 0.0532276488732942, 0.289724590658638, "prenumb", 240, -0.000745866258043459,
			 0.0874049918617715, 0.0449757540521855, 0.175555849981586, "postnumb",
			 240, -0.039651059202616, 0.000534051541867305, 0.0205029842698429,
			 0.0407191622863506, "funumb"))
})

test_that("Bain ANCOVA table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.000105084311355764, 0.000105084311355764, 0.012382991414958,
			 0.000104200050823146, "H1", 0.00832657849517309, 0.00838109292000752,
			 0.987616517988688, 0.00831056793303519, "H2", 4.13483006346101e-09,
			 4.16328965451245e-09, 4.90596354343274e-07, 4.12825652083301e-09,
			 "H3", "", "", "", 0.991585227887885, "Hu"))
})

test_that("Posterior Probabilities plot matches", {
	skip("Does not reproduce on Linux")
	plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_posteriorProbabilityPlot"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "posterior-probabilities", dir="BainAncovaBayesian")
})

test_that("Hypothesis Legend table results match", {
	table <- results[["results"]][["legendTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("site1 = site2 = site3 = site4 = site5", "H1", "site1 &lt; site2 &lt; site3 &lt; site4 &lt; site5",
			 "H2", "site1 &gt; site2 &gt; site3 &gt; site4 &gt; site5", "H3"
			))
})

# ==================================================================================================

options <- jaspTools::analysisOptions("BainAncovaBayesian")
options$dependent <- "postnumb"
options$fixedFactors <- "site"
options$covariates <- list("prenumb", "funumb")
options$bayesFactorMatrix <- TRUE
options$descriptives <- TRUE
options$seed <- 900
options$model <- "site1 = site2 = site3 = site4 = site5;(site1, site3, site4) < (site2, site5)"
set.seed(1)
results <- jaspTools::runAnalysis("BainAncovaBayesian", "sesame.csv", options)


test_that("Bayes Factor Matrix table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 0.000151817953145536, "H1", 6586.83626857606, 1, "H2"))
})

test_that("Coefficients for Groups plus Covariates table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(60, 25.4137483833314, 27.4900972831131, 1.05938114993933, 29.5664461828948,
			 "site1", 55, 32.1234899760836, 34.3533531246737, 1.13770618551105,
			 36.5832162732638, "site2", 64, 25.866699539707, 27.9637950358383,
			 1.06996634258228, 30.0608905319696, "site3", 43, 24.6157064736522,
			 27.0489049018369, 1.24145058142776, 29.4821033300216, "site4",
			 18, 28.2756928301543, 32.0207748935292, 1.91079126602102, 35.7658569569041,
			 "site5", 240, 0.511117765271732, 0.621794061201558, 0.0564685355459723,
			 0.732470357131383, "prenumb", 240, 0.131745579989435, 0.18492114241481,
			 0.0271308875289635, 0.238096704840186, "funumb"))
})

test_that("Bain ANCOVA table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.00140342160354946, 0.00140342160354946, 0.000151794907953313,
			 0.000136979161135984, "H1", 172.600145394316, 9.24410831836275,
			 0.999848205092047, 0.902259306609623, "H2", "", "", "", 0.0976037142292406,
			 "Hu"))
})

test_that("Hypothesis Legend table results match", {
	table <- results[["results"]][["legendTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("site1 = site2 = site3 = site4 = site5", "H1", "(site1, site3, site4) &lt; (site2, site5)",
			 "H2"))
})

# ==================================================================================================