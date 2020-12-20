context("Bain ANCOVA")

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
		list(1, 0.0125382587162262, 25240.6918749608, "H1", 79.7558913588109,
			 1, 2013093.87900059, "H2", 3.96185653291072e-05, 4.96747822062056e-07,
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
		list(60, -3.01550164109298, -1.54226715607096, 0.751664059463699, -0.0690326710489346,
			 "site1", 55, -4.95229562620438, -3.15628448711012, 0.91634905195247,
			 -1.36027334801586, "site2", 64, 1.91729451986608, 3.40182478351333,
			 0.757427317724733, 4.88635504716058, "site3", 43, -0.21833975516451,
			 1.47068363556364, 0.861762463010012, 3.1597070262918, "site4",
			 18, -3.40802758727219, -0.823583684376468, 1.31861805792427,
			 1.76086021851925, "site5", 240, 0.00400716421086941, 0.0687595426462478,
			 0.0330375348456078, 0.133511921081626, "peabody", 240, 0.0810760411118375,
			 0.185400315885238, 0.0532276488732942, 0.289724590658639, "prenumb",
			 240, -0.000745866258043806, 0.0874049918617711, 0.0449757540521855,
			 0.175555849981586, "postnumb", 240, -0.0396510592026159, 0.000534051541867433,
			 0.0205029842698429, 0.0407191622863507, "funumb"))
})

test_that("Bain ANCOVA table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.000105084311355787, 0.0123829914149606, 0.00010420005082317,
			 "H1", 0.00832657849517319, 0.987616517988685, 0.00831056793303529,
			 "H2", 4.13483006346205e-09, 4.9059635434339e-07, 4.12825652083404e-09,
			 "H3", "", "", 0.991585227887885, "Hu"))
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