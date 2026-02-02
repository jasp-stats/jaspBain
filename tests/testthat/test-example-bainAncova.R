context("Example: bainAncova")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("BainAncovaBayesian results match", {

  testthat::skip("Encoding of VariableLevels is not implemented in jaspTools")

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "bainAncova.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset, forceEncode = c("model", "hypothesis"))
  set.seed(1)
  results <- jaspTools::runAnalysis("BainAncovaBayesian", encoded$dataset, encoded$options, encodedDataset = TRUE)
})

