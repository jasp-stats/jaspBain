context("Bain ANCOVA")

# ==================================================================================================

# library(bain)
# data("sesamesim")
# set.seed(100)
# sesamesim$site <- as.factor(sesamesim$site)
# tt <- lm(age ~ site+peabody+prenumb+postnumb+funumb-1, sesamesim)
# bainResult <- bain(tt,"site1 = site2 = site3 = site4 = site5;
#                         site1 < site2 < site3 < site4 < site5;
#                         site1 > site2 > site3 > site4 > site5", fraction = 1)

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
	skip_on_os("linux")
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 0.0145099077084855, 28921.6664220172, "H1", 68.918425953543,
			 1, 1993235.72575887, "H2", 3.457615427162e-05, 5.01696807395563e-07,
			 1, "H3"))
})

test_that("Adjusted Means plot matches", {
	plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesPlots"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "adjusted-means")
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
	skip_on_os("linux")
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.000105084311355764, 0.000105084311355764, 0.0143023744004669,
			 0.000104317853320798, 0.000102906747473304, "H1", 0.00719537689377166,
			 0.00724224533105126, 0.985697131078429, 0.00718942224972197,
			 0.00709217105585884, "H2", 3.60742104437074e-09, 3.63341136096384e-09,
			 4.94521103721013e-07, 3.60691018970414e-09, 3.5581195762276e-09,
			 "H3", "", "", "", 0.992706256290047, "", "Hu", "", 1.01381322165305,
			 "", "", 0.992804918638548, "Hc"))
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
		list("site1 = site2 = site3 = site4 = site5", "H1", "site1 &lt; site2 &lt; site3 &lt; site4 &lt; site5",
			 "H2", "site1 &gt; site2 &gt; site3 &gt; site4 &gt; site5", "H3"
			))
})

# ==================================================================================================

# library(bain)
# data("sesamesim")
# set.seed(900)
# sesamesim$site <- as.factor(sesamesim$site)
# tt <- lm(postnumb ~ site+prenumb+funumb-1, sesamesim)
# bainResult <- bain(tt,"site1 = site2 = site3 = site4 = site5; 
#               (site1, site3, site4) < (site2, site5)", fraction = 1)

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
	skip_on_os("linux")
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 0.000148187711582985, "H1", 6748.19787226418, 1, "H2"))
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
	skip_on_os("linux")
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0.00140342160354946, 0.00140342160354946, 0.000148165755238781,
			 0.000134016960521465, 0.000147430194985523, "H1", 200.402822510065,
			 9.47056667896204, 0.999851834244761, 0.904372967838261, 0.994888128108796,
			 "H2", "", "", "", 0.095493015201217, "", "Hu", "", 0.0472576511664969,
			 "", "", 0.00496444169621827, "Hc"))
})

test_that("Hypothesis Legend table results match", {
	table <- results[["results"]][["legendTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("site1 = site2 = site3 = site4 = site5", "H1", "(site1, site3, site4) &lt; (site2, site5)",
			 "H2"))
})

# ==================================================================================================