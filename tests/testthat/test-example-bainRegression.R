context("Example: bainRegression")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("BainRegressionLinearBayesian results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "bainRegression.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset, forceEncode = c("model", "hypothesis"))
  set.seed(1)
  results <- jaspTools::runAnalysis("BainRegressionLinearBayesian", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 2.19639931243443e-20, "H1", 45529061784836694016, 1, "H2"
    ))

  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(240, -0.208514869778307, -0.0589396581503505, 0.0763152857949364,
     0.0906355534776059, "jaspColumn1", 240, 0.388717681928869, 0.526688174616913,
     0.0703944020279644, 0.664658667304956, "jaspColumn2"))

  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(7.49229638532845e-20, 7.49229638532845e-20, 2.19639931243443e-20,
     1.69848193633433e-20, 1.75572369718977e-20, "H1", 3.98416348999712,
     3.41117225037928, 1, 0.773302890197947, 0.799364526864551, "H2",
     "", "", "", 0.226697109802053, "", "Hu", "", 0.856182799461813,
     "", "", 0.200635473135449, "Hc"))

  plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_posteriorProbabilityPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_posterior-probabilities")

  table <- results[["results"]][["legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("jaspColumn1 = jaspColumn2 = 0", "H1", "jaspColumn2 &gt; jaspColumn1 &gt; 0",
     "H2"))

})

