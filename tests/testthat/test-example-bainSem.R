context("Example: bainSem")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("BainSemBayesian (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "bainSem.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset, forceEncode = c("model", "hypothesis"))
  set.seed(1)
  results <- jaspTools::runAnalysis("BainSemBayesian", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, "H1"))

  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(240, 0.643333953778611, 0.710450838060891, 0.034243937547674,
     0.777567722343171, "A=~jaspColumn1", 240, 0.763294389351268,
     0.811402927163143, 0.024545623384587, 0.859511464975018, "A=~jaspColumn4",
     240, 0.79412258038196, 0.837093624928896, 0.0219244051859553,
     0.880064669475831, "A=~jaspColumn3", 240, 0.87697273865119,
     0.906133263733766, 0.0148780923081191, 0.935293788816342, "A=~jaspColumn5",
     240, 0.628742928228994, 0.698041432162551, 0.0353570292516468,
     0.767339936096108, "A=~jaspColumn6", 240, 0.837424921519763,
     0.873117007119152, 0.0182105823785147, 0.90880909271854, "A=~jaspColumn2",
     240, 0.707618804453303, 0.765839653093138, 0.0297050604496174,
     0.824060501732973, "B=~jaspColumn7", 240, 0.568799850146867,
     0.647669783233517, 0.0402405012075557, 0.726539716320167, "B=~jaspColumn10",
     240, 0.760416046900686, 0.810184863734924, 0.0253927200840463,
     0.859953680569161, "B=~jaspColumn9", 240, 0.853115565947065,
     0.887898658040638, 0.0177468016595907, 0.922681750134212, "B=~jaspColumn11",
     240, 0.654077823365769, 0.720567689980902, 0.0339240246961663,
     0.787057556596036, "B=~jaspColumn12", 240, 0.781930331790296,
     0.828200406721732, 0.0236076148829306, 0.874470481653168, "B=~jaspColumn8"
    ))

  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(717.77444000552, 89.1148992929482, 1, 0.988903055900344, 0.998608742959763,
     "H1", "", "", "", 0.0110969440996563, "", "Hu", "", 0.124154461800371,
     "", "", 0.00139125704023688, "Hc"))

  plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_pathDiagram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_path-diagram")

  table <- results[["results"]][["legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("A=~jaspColumn1 &gt; .6 &amp; A=~jaspColumn4 &gt; .6 &amp; A=~jaspColumn3 &gt; .6 &amp; A=~jaspColumn5 &gt; .6 &amp; A=~jaspColumn6 &gt; .6 &amp; A=~jaspColumn2 &gt;.6 &amp; B=~jaspColumn7 &gt; .6 &amp; B=~jaspColumn10 &gt; .6 &amp; B=~jaspColumn9 &gt; .6 &amp; B=~jaspColumn11 &gt; .6 &amp; B=~jaspColumn12 &gt; .6 &amp; B=~jaspColumn8 &gt;.6",
     "H1"))

})

test_that("BainSemBayesian (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "bainSem.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset, forceEncode = c("model", "hypothesis"))
  set.seed(1)
  results <- jaspTools::runAnalysis("BainSemBayesian", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 2.76108910965972, 8.08262818027485, "H1", 0.362175924167561,
     1, 2.92733333089382, "H2", 0.123722133159662, 0.341607834491013,
     1, "H3"))

  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(240, 0.644241710795128, 0.711205262171606, 0.0341657050357443,
     0.778168813548083, "A=~jaspColumn1", 240, 0.763763719563103,
     0.811775527594914, 0.0244962705491135, 0.859787335626726, "A=~jaspColumn4",
     240, 0.794960087713298, 0.837770094339869, 0.0218422414719101,
     0.88058010096644, "A=~jaspColumn3", 240, 0.877193064932545,
     0.906287870649154, 0.0148445614032223, 0.935382676365763, "A=~jaspColumn5",
     240, 0.629537996408956, 0.698701975059689, 0.0352883926420534,
     0.767865953710422, "A=~jaspColumn6", 240, 0.837895310881132,
     0.873484275245374, 0.0181579685366482, 0.909073239609616, "A=~jaspColumn2",
     240, 0.707718711922821, 0.765919627657576, 0.0296948904132099,
     0.824120543392331, "B=~jaspColumn7", 240, 0.568915623633037,
     0.647765958992103, 0.0402305021832174, 0.726616294351169, "B=~jaspColumn10",
     240, 0.760495385574049, 0.810246502727717, 0.0253836894688363,
     0.859997619881385, "B=~jaspColumn9", 240, 0.853011515071321,
     0.88780309732995, 0.0177511334560531, 0.922594679588578, "B=~jaspColumn11",
     240, 0.65386367404815, 0.720382072541198, 0.0339385820442301,
     0.786900471034247, "B=~jaspColumn12", 240, 0.78202772323052,
     0.828276665034914, 0.0235968324771269, 0.874525606839307, "B=~jaspColumn8",
     240, 0.729997143600514, 0.788751775228553, 0.0299774037132764,
     0.847506406856592, "A~B", 240, -0.0926531058196265, -0.000478606046975604,
     0.047028670169305, 0.0916958937256753, "A~jaspColumn13", 240,
     -0.107692732546452, -0.0155271511198026, 0.0470241199091613,
     0.0766384303068465, "A~jaspColumn14"))

  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(50.2904553874892, 50.2904553874892, 0.67299367885221, 0.664106515599475,
     0.66462048995971, "H1", 18.2139921567714, 18.2139921567714,
     0.243742107597226, 0.240523391032939, 0.240709540171856, "H2",
     6.6090832437396, 6.22204241811098, 0.0832642135505638, 0.0821646747551973,
     0.082228264759435, "H3", "", "", "", 0.0132054186123891, "",
     "Hu", "", 0.941438046495292, "", "", 0.012441705108999, "Hc"
    ))

  plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_pathDiagram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_path-diagram")

  table <- results[["results"]][["legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("A~B &gt; A~jaspColumn14 = A~jaspColumn13 = 0", "H1", "A~B &gt; A~jaspColumn14 &gt; A~jaspColumn13 = 0",
     "H2", "A~B &gt; A~jaspColumn14 &gt; A~jaspColumn13 &gt; 0",
     "H3"))

})

test_that("BainSemBayesian (analysis 3) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "bainSem.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[3]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset, forceEncode = c("model", "hypothesis"))
  set.seed(1)
  results <- jaspTools::runAnalysis("BainSemBayesian", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, 84.7494670985511, "H1", 0.0117994842237432, 1, "H2"))

  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(115, 0.381341176885363, 0.531813744650272, 0.0767731289716632,
     0.68228631241518, "jaspColumn3~jaspColumn4.1", 115, 0.0651184114848468,
     0.230979720241261, 0.0846246717106575, 0.396841028997675, "jaspColumn3~jaspColumn2.1",
     115, 0.181822706221455, 0.61184250948288, 0.219401890368071,
     1.0418623127443, "jaspColumn3~1.1", 125, 0.515320236874534,
     0.637881043952246, 0.0625321730625953, 0.760441851029959, "jaspColumn3~jaspColumn4.2",
     125, -0.0907111262798205, 0.0629819922557704, 0.0784162973135745,
     0.216675110791361, "jaspColumn3~jaspColumn2.2", 125, 0.474400147477896,
     0.966070060702074, 0.250856606091953, 1.45773997392625, "jaspColumn3~1.2"
    ))

  table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(13.7330959852909, 13.7330959852909, 0.988338119945973, 0.921985057305775,
     0.916102818621242, "H1", 0.14789834045115, 0.162043449421592,
     0.0116618800540266, 0.0108789481382065, 0.0108095407556481,
     "H2", "", "", "", 0.0671359945560188, "", "Hu", "", 1.09564075517881,
     "", "", 0.0730876406231101, "Hc"))

  plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_pathDiagram"]][["collection"]][["bainContainer_pathDiagram_pathDiagram1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-1_group-1")

  plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_pathDiagram"]][["collection"]][["bainContainer_pathDiagram_pathDiagram2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-3_figure-2_group-2")

  table <- results[["results"]][["legendTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("jaspColumn3~jaspColumn4.1 = jaspColumn3~jaspColumn4.2 &amp; jaspColumn3~jaspColumn2.1 = jaspColumn3~jaspColumn2.2",
     "H1", "jaspColumn3~jaspColumn4.1 &lt; jaspColumn3~jaspColumn4.2 &amp; jaspColumn3~jaspColumn2.1 &lt; jaspColumn3~jaspColumn2.2",
     "H2"))

})

