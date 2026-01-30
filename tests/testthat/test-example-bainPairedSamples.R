context("Example: bainPairedSamples")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("BainTTestBayesianPairedSamples results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "bainPairedSamples.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset, forceEncode = c("model", "hypothesis"))
  set.seed(1)
  results <- jaspTools::runAnalysis("BainTTestBayesianPairedSamples", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesPlots"]][["collection"]][["bainContainer_descriptivesPlots_jaspColumn1 - jaspColumn2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_jaspcolumn1-jaspcolumn2")

  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(240, 7.48733943597818, 8.69166666666667, 9.51923473009165, 0.61446395963807,
     9.89599389735516, "jaspColumn1 - jaspColumn2"))

  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("", 2.76244529575416e-43, 276.004579526046, "jaspColumn1 - jaspColumn2",
     2.76244529575416e-43, 1, 1.00086936981184e-45, "H0: Equal",
     "H1: Bigger", "H2: Smaller"))

  plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_posteriorProbabilityPlot"]][["collection"]][["bainContainer_posteriorProbabilityPlot_jaspColumn1 - jaspColumn2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_jaspcolumn1-jaspcolumn2")

})

