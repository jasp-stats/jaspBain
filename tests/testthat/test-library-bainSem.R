context("Library: bainSem")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("BainSemBayesian (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "bainSem.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("BainSemBayesian", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(".", "H1"))

  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("H1", "H0", "H-1", "H-2", "", "", "", "", "Hu", "", "", "", "Hc"
    ))

  plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_pathDiagram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_path-diagram")

  table <- results[["results"]][["legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("A=~Ab &gt; .6 &amp; A=~Al &gt; .6 &amp; A=~Af &gt; .6 &amp; A=~An &gt; .6 &amp; A=~Ar &gt; .6 &amp; A=~Ac &gt;.6 &amp; B=~Bb &gt; .6 &amp; B=~Bl &gt; .6 &amp; B=~Bf &gt; .6 &amp; B=~Bn &gt; .6 &amp; B=~Br &gt; .6 &amp; B=~Bc &gt;.6",
     "H1"))

})

test_that("BainSemBayesian (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "bainSem.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("BainSemBayesian", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(".", "H1"))

  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("H1", "H0", "H-1", "H-2", "", "", "", "", "Hu", "", "", "", "Hc"
    ))

  plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_pathDiagram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_path-diagram")

  table <- results[["results"]][["legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("A~B &gt; A~peabody = A~age = 0", "H1", "A~B &gt; A~peabody &gt; A~age = 0",
     "H2", "A~B &gt; A~peabody &gt; A~age &gt; 0", "H3"))

})

test_that("BainSemBayesian (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "bainSem.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("BainSemBayesian", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(".", "H1"))

  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("H1", "H0", "H-1", "H-2", "", "", "", "", "Hu", "", "", "", "Hc"
    ))

  plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_pathDiagram"]][["collection"]][["bainContainer_pathDiagram_pathDiagram1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-1_group-1")

  plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_pathDiagram"]][["collection"]][["bainContainer_pathDiagram_pathDiagram2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-2_group-2")

  table <- results[["results"]][["legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("postnumb~prenumb.1 = postnumb~prenumb.2 &amp; postnumb~peabody.1 = postnumb~peabody.2",
     "H1", "postnumb~prenumb.1 &lt; postnumb~prenumb.2 &amp; postnumb~peabody.1 &lt; postnumb~peabody.2",
     "H2"))

})

