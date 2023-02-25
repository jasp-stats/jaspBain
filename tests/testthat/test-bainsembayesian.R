context("Bain SEM")

# ==================================================================================================

# library(bain)
# library(lavaan)
# data("sesamesim")
# model1 <- '
#     A =~ Ab + Al + Af + An + Ar + Ac 
#     B =~ Bb + Bl + Bf + Bn + Br + Bc 
# '
# fit1 <- sem(model1, data = sesamesim, std.lv = TRUE)
# hypotheses1 <-
#   " A=~Ab > .6 & A=~Al > .6 & A=~Af > .6 & A=~An > .6 & A=~Ar > .6 & A=~Ac >.6 & 
#     B=~Bb > .6 & B=~Bl > .6 & B=~Bf > .6 & B=~Bn > .6 & B=~Br > .6 & B=~Bc >.6"
# set.seed(100)
# bainResult <- bain(fit1,hypotheses1,fraction=3,standardize=FALSE)

options <- jaspTools::analysisOptions("BainSemBayesian")
options$syntax <- list("", model = "A =~ Ab + Al + Af + An + Ar + Ac; B =~ Bb + Bl + Bf + Bn + Br + Bc")
options$bayesFactorMatrix <- TRUE
options$descriptives <- TRUE
options$bayesFactorPlot <- TRUE
options$fixedFactors <- ""
options$pathDiagram <- TRUE
options$pathDiagramEstimates <- TRUE
options$pathDiagramLegend <- TRUE
options$model <- "A=~Ab > .6 & A=~Al > .6 & A=~Af > .6 & A=~An > .6 & A=~Ar > .6 & A=~Ac >.6 & B=~Bb > .6 & B=~Bl > .6 & B=~Bf > .6 & B=~Bn > .6 & B=~Br > .6 & B=~Bc >.6"
set.seed(1)
results <- jaspTools::runAnalysis("BainSemBayesian", "sesame.csv", options)


test_that("Bayes Factor Matrix table results match", {
	skip_on_os("linux")
	skip_on_os("windows")
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, "H1"))
})

test_that("Coefficients for Parameters table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(240, 3.27234343764435, 3.88479958570525, 0.312483368516908, 4.49725573376614,
			 "A=~Ab", 240, 8.33433831455941, 9.58335890609076, 0.637267113775286,
			 10.8323794976221, "A=~Al", 240, 2.94916536411093, 3.36743845907787,
			 0.21340856172166, 3.78571155404482, "A=~Af", 240, 9.71650568863459,
			 10.9063769996814, 0.607088354904675, 12.0962483107282, "A=~An",
			 240, 1.50072596736168, 1.78966463397027, 0.147420395929569,
			 2.07860330057886, "A=~Ar", 240, 3.91023942823755, 4.42382724638746,
			 0.262039416132655, 4.93741506453737, "A=~Ac", 240, 3.80014730487736,
			 4.43337130535907, 0.323079406293429, 5.06659530584077, "B=~Bb",
			 240, 4.14813162554953, 5.05396312117863, 0.462167418776155,
			 5.95979461680774, "B=~Bl", 240, 2.77927605023721, 3.19986341663871,
			 0.214589334150548, 3.62045078304022, "B=~Bf", 240, 7.61169331094647,
			 8.58805986628854, 0.498155355426693, 9.56442642163061, "B=~Bn",
			 240, 1.68630422453705, 1.99705693902588, 0.158550216708065,
			 2.30780965351471, "B=~Br", 240, 3.21686768601433, 3.68521736226071,
			 0.238958307367205, 4.15356703850708, "B=~Bc"))
})

test_that("Bain Structural Equation Model table results match", {
	skip_on_os("linux")
	skip_on_os("windows")
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(48171725616844.4, 26.2218088646654, 1, 0.963264748313694, 0.999999999999979,
			 "H1", "", "", "", 0.0367352516863061, "", "Hu", "", 5.44340243761088e-13,
			 "", "", 2.0759065347875e-14, "Hc"))
})

test_that("Path Diagram plot matches", {
	plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_pathDiagram"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "path-diagram")
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
		list("A=~Ab &gt; .6 &amp; A=~Al &gt; .6 &amp; A=~Af &gt; .6 &amp; A=~An &gt; .6 &amp; A=~Ar &gt; .6 &amp; A=~Ac &gt;.6 &amp; B=~Bb &gt; .6 &amp; B=~Bl &gt; .6 &amp; B=~Bf &gt; .6 &amp; B=~Bn &gt; .6 &amp; B=~Br &gt; .6 &amp; B=~Bc &gt;.6",
			 "H1"))
})

# ==================================================================================================

# library(bain)
# library(lavaan)
# data("sesamesim")
# model1 <- '
#     A =~ Ab + Al + Af + An + Ar + Ac 
#     B =~ Bb + Bl + Bf + Bn + Br + Bc 
# '
# fit1 <- sem(model1, data = sesamesim, std.lv = TRUE)
# hypotheses1 <-
#   " A=~Ab > .6 & A=~Al > .6 & A=~Af > .6 & A=~An > .6 & A=~Ar > .6 & A=~Ac >.6 & 
#     B=~Bb > .6 & B=~Bl > .6 & B=~Bf > .6 & B=~Bn > .6 & B=~Br > .6 & B=~Bc >.6"
# set.seed(100)
# bainResult <- bain(fit1,hypotheses1,fraction=3,standardize=TRUE)

options <- jaspTools::analysisOptions("BainSemBayesian")
options$syntax <- list("", model = "A =~ Ab + Al + Af + An + Ar + Ac;B =~ Bb + Bl + Bf + Bn + Br + Bc")
options$fixedFactors <- ""
options$descriptives <- TRUE
options$pathDiagram <- TRUE
options$pathDiagramEstimates <- TRUE
options$pathDiagramLegend <- TRUE
options$fraction <- 3
options$standardized <- TRUE
options$bayesFactorMatrix <- TRUE
options$model <- "A=~Ab > .6 & A=~Al > .6 & A=~Af > .6 & A=~An > .6 & A=~Ar > .6 & A=~Ac >.6 & B=~Bb > .6 & B=~Bl > .6 & B=~Bf > .6 & B=~Bn > .6 & B=~Br > .6 & B=~Bc >.6"
set.seed(1)
results <- jaspTools::runAnalysis("BainSemBayesian", "sesame.csv", options)


test_that("Bayes Factor Matrix table results match", {
	skip_on_os("linux")
	skip_on_os("windows")
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, "H1"))
})

test_that("Coefficients for Parameters table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(240, 0.643333953778611, 0.710450838060891, 0.034243937547674,
			 0.777567722343171, "A=~Ab", 240, 0.763294389351268, 0.811402927163143,
			 0.0245456233845871, 0.859511464975018, "A=~Al", 240, 0.7941225785561,
			 0.837093624928896, 0.0219244061175337, 0.880064671301692, "A=~Af",
			 240, 0.876972738651191, 0.906133263733766, 0.0148780923081191,
			 0.935293788816342, "A=~An", 240, 0.628742928228994, 0.698041432162551,
			 0.0353570292516469, 0.767339936096108, "A=~Ar", 240, 0.837424920259227,
			 0.873117007119152, 0.0182105830216572, 0.908809093979076, "A=~Ac",
			 240, 0.707618804453302, 0.765839653093138, 0.0297050604496174,
			 0.824060501732973, "B=~Bb", 240, 0.568799849404187, 0.647669783233517,
			 0.040240501586481, 0.726539717062846, "B=~Bl", 240, 0.760416045819062,
			 0.810184863734924, 0.0253927206359054, 0.859953681650785, "B=~Bf",
			 240, 0.853115564416809, 0.887898658040638, 0.0177468024403481,
			 0.922681751664468, "B=~Bn", 240, 0.654077823365769, 0.720567689980902,
			 0.0339240246961663, 0.787057556596036, "B=~Br", 240, 0.781930330642664,
			 0.828200406721732, 0.0236076154684682, 0.874470482800801, "B=~Bc"
			))
})

test_that("Bain Structural Equation Model table results match", {
	skip_on_os("linux")
	skip_on_os("windows")
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(771.696358400003, 91.2565818546305, 1, 0.989160664963984, 0.998705830577394,
			 "H1", "", "", "", 0.0108393350360163, "", "Hu", "", 0.118254519230643,
			 "", "", 0.00129416942260562, "Hc"))
})

test_that("Path Diagram plot matches", {
	plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_pathDiagram"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "path-diagram-2")
})

test_that("Hypothesis Legend table results match", {
	table <- results[["results"]][["legendTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("A=~Ab &gt; .6 &amp; A=~Al &gt; .6 &amp; A=~Af &gt; .6 &amp; A=~An &gt; .6 &amp; A=~Ar &gt; .6 &amp; A=~Ac &gt;.6 &amp; B=~Bb &gt; .6 &amp; B=~Bl &gt; .6 &amp; B=~Bf &gt; .6 &amp; B=~Bn &gt; .6 &amp; B=~Br &gt; .6 &amp; B=~Bc &gt;.6",
			 "H1"))
})

# ==================================================================================================

# library(bain)
# library(lavaan)
# data("sesamesim")
# model2 <- '
#     A  =~ Ab + Al + Af + An + Ar + Ac 
#     B =~ Bb + Bl + Bf + Bn + Br + Bc
#     A ~ B + age + peabody
# '
# fit2 <- sem(model2, data = sesamesim, std.lv = TRUE)
# hypotheses2 <- "A~B > A~peabody = A~age = 0; 
#                 A~B > A~peabody > A~age = 0; 
#                 A~B > A~peabody > A~age > 0"
# set.seed(100)
# bainResult <- bain(fit2, hypotheses2, fraction = 3, standardize = TRUE)

options <- jaspTools::analysisOptions("BainSemBayesian")
options$syntax <- list("", model = "A  =~ Ab + Al + Af + An + Ar + Ac;B =~ Bb + Bl + Bf + Bn + Br + Bc;A ~ B + age + peabody")
options$fixedFactors <- ""
options$descriptives <- TRUE
options$fraction <- 3
options$standardized <- TRUE
options$bayesFactorMatrix <- TRUE
options$model <- "A~B > A~peabody = A~age = 0;A~B > A~peabody > A~age = 0;A~B > A~peabody > A~age > 0"
set.seed(1)
results <- jaspTools::runAnalysis("BainSemBayesian", "sesame.csv", options)


test_that("Bayes Factor Matrix table results match", {
	skip_on_os("linux")
	skip_on_os("windows")
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 2.38776763973124, 9.19788323622478, "H1", 0.41880121975041,
			 1, 3.85208471845279, "H2", 0.108720666953198, 0.259599690320844,
			 1, "H3"))
})

test_that("Coefficients for Parameters table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(240, 0.644241710170201, 0.711205262171606, 0.0341657053545905,
			 0.77816881417301, "A=~Ab", 240, 0.763763717887125, 0.811775527594914,
			 0.0244962714042199, 0.859787337302704, "A=~Al", 240, 0.79496008853169,
			 0.837770094339869, 0.0218422410543558, 0.880580100148049, "A=~Af",
			 240, 0.877193065789862, 0.906287870649154, 0.0148445609658073,
			 0.935382675508445, "A=~An", 240, 0.629537994391315, 0.698701975059689,
			 0.0352883936714812, 0.767865955728063, "A=~Ar", 240, 0.837895313363849,
			 0.873484275245374, 0.0181579672699324, 0.909073237126898, "A=~Ac",
			 240, 0.707718712745665, 0.765919627657576, 0.0296948899933838,
			 0.824120542569488, "B=~Bb", 240, 0.568915624824277, 0.647765958992103,
			 0.0402305015754307, 0.726616293159929, "B=~Bl", 240, 0.760495386365103,
			 0.810246502727717, 0.02538368906523, 0.859997619090331, "B=~Bf",
			 240, 0.853011515371342, 0.88780309732995, 0.0177511333029788,
			 0.922594679288557, "B=~Bn", 240, 0.653863673320625, 0.720382072541199,
			 0.0339385824154234, 0.786900471761773, "B=~Br", 240, 0.782027722437893,
			 0.828276665034914, 0.0235968328815362, 0.874525607631935, "B=~Bc",
			 240, 0.729997143600828, 0.788751775228554, 0.0299774037131166,
			 0.847506406856279, "A~B", 240, -0.0926531058198714, -0.00047860604697559,
			 0.0470286701694299, 0.0916958937259202, "A~age", 240, -0.107692732546606,
			 -0.0155271511198026, 0.04702411990924, 0.0766384303070007, "A~peabody"
			))
})

test_that("Bain Structural Equation Model table results match", {
	skip_on_os("linux")
	skip_on_os("windows")
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(50.2904553875014, 50.2904553875014, 0.654655104260404, 0.64624265208595,
			 0.646668886707785, "H1", 21.0617040580892, 21.0617040580892,
			 0.274170356180089, 0.270647210948336, 0.27082571852786, "H2",
			 5.76322394871271, 5.46761185110921, 0.0711745395595067, 0.0702599321483882,
			 0.0703062726607526, "H3", "", "", "", 0.0128502048173252, "",
			 "Hu", "", 0.948707164560294, "", "", 0.0121991221036025, "Hc"
			))
})

test_that("Hypothesis Legend table results match", {
	table <- results[["results"]][["legendTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("A~B &gt; A~peabody = A~age = 0", "H1", "A~B &gt; A~peabody &gt; A~age = 0",
			 "H2", "A~B &gt; A~peabody &gt; A~age &gt; 0", "H3"))
})

# ==================================================================================================

# library(bain)
# library(lavaan)
# data("sesamesim")
# model2 <- '
#     A  =~ Ab + Al + Af + An + Ar + Ac 
#     B =~ Bb + Bl + Bf + Bn + Br + Bc
#     A ~ B + age + peabody
# '
# fit2 <- sem(model2, data = sesamesim, std.lv = TRUE)
# hypotheses2 <- "A~B > A~peabody = A~age = 0; 
#                 A~B > A~peabody > A~age = 0; 
#                 A~B > A~peabody > A~age > 0"
# set.seed(100)
# bainResult <- bain(fit2, hypotheses2, fraction = 3, standardize = FALSE)

options <- jaspTools::analysisOptions("BainSemBayesian")
options$syntax <- list("", model = "A  =~ Ab + Al + Af + An + Ar + Ac;B =~ Bb + Bl + Bf + Bn + Br + Bc;A ~ B + age + peabody")
options$fixedFactors <- ""
options$descriptives <- TRUE
options$fraction <- 3
options$bayesFactorMatrix <- TRUE
options$model <- "A~B > A~peabody = A~age = 0;A~B > A~peabody > A~age = 0;A~B > A~peabody > A~age > 0"
set.seed(1)
results <- jaspTools::runAnalysis("BainSemBayesian", "sesame.csv", options)


test_that("Bayes Factor Matrix table results match", {
	skip_on_os("linux")
	skip_on_os("windows")
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 6.43739309620418, 41.8063151520964, "H1", 0.155342385505346,
			 1, 6.49429272491492, "H2", 0.0239198311633513, 0.153981355993327,
			 1, "H3"))
})

test_that("Coefficients for Parameters table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(240, 1.97776463189764, 2.39194578953625, 0.211320800231845, 2.80612694717485,
			 "A=~Ab", 240, 5.02018110926225, 5.89871306151199, 0.448238824376104,
			 6.77724501376173, "A=~Al", 240, 1.7758399066208, 2.07359460158627,
			 0.15191845223388, 2.37134929655174, "A=~Af", 240, 5.83194029212427,
			 6.71305697054836, 0.449557586452725, 7.59417364897245, "A=~An",
			 240, 0.907164198528248, 1.1017689666239, 0.0992899714641016,
			 1.29637373471954, "A=~Ar", 240, 2.35075889927942, 2.72333757649134,
			 0.190094654876708, 3.09591625370326, "A=~Ac", 240, 3.80065355470599,
			 4.43383291130091, 0.323056628381623, 5.06701226789583, "B=~Bb",
			 240, 4.14894152081647, 5.05471337859966, 0.462136990744625,
			 5.96048523638284, "B=~Bl", 240, 2.77954352142843, 3.20010329873907,
			 0.214575257825122, 3.62066307604971, "B=~Bf", 240, 7.61072801303949,
			 8.58713247359074, 0.498174695174508, 9.563536934142, "B=~Bn",
			 240, 1.68576714827575, 1.99654229693431, 0.158561662923354,
			 2.30731744559286, "B=~Br", 240, 3.21724343103262, 3.6855573294566,
			 0.238940053040763, 4.15387122788059, "B=~Bc", 240, 1.03048850664578,
			 1.28354041258156, 0.12911048770887, 1.53659231851735, "A~B",
			 240, -0.0240141524531994, -0.000124046735372882, 0.0121890534245877,
			 0.0237660589824537, "A~age", 240, -0.010927740449043, -0.00157515580214844,
			 0.00477181454387254, 0.00777742884474616, "A~peabody"))
})

test_that("Bain Structural Equation Model table results match", {
	skip_on_os("linux")
	skip_on_os("windows")
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(50.2923775087579, 50.2923775087579, 0.84798782311953, 0.833926849102382,
			 0.834001234309521, "H1", 7.81253789494585, 7.81253789494585,
			 0.131728451322873, 0.12954418607652, 0.129555741252044, "H2",
			 1.20949091885259, 1.20298517881302, 0.020283725557597, 0.0199473894331145,
			 0.0199491687147103, "H3", "", "", "", 0.0165815753879832, "",
			 "Hu", "", 0.994621092280917, "", "", 0.0164938557237251, "Hc"
			))
})

test_that("Hypothesis Legend table results match", {
	table <- results[["results"]][["legendTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("A~B &gt; A~peabody = A~age = 0", "H1", "A~B &gt; A~peabody &gt; A~age = 0",
			 "H2", "A~B &gt; A~peabody &gt; A~age &gt; 0", "H3"))
})

# ==================================================================================================

# library(bain)
# library(lavaan)
# data("sesamesim")
# model3 <- '
#     postnumb ~ prenumb + peabody 
# '
# sesamesim$sex <- factor(sesamesim$sex, labels = c("boy", "girl"))
# fit3 <- sem(model3, data = sesamesim, std.lv = TRUE, group = "sex")
# hypotheses3 <-
#   "postnumb~prenumb.boy = postnumb~prenumb.girl & postnumb~peabody.boy = postnumb~peabody.girl;
#    postnumb~prenumb.boy < postnumb~prenumb.girl & postnumb~peabody.boy < postnumb~peabody.girl"
# set.seed(100)
# bainResult <- bain(fit3, hypotheses3, fraction = 3, standardize = TRUE)

options <- jaspTools::analysisOptions("BainSemBayesian")
options$syntax <- list("", model = "postnumb ~ prenumb + peabody")
options$fixedFactors <- "sex"
options$descriptives <- TRUE
options$fraction <- 3
options$standardized <- TRUE
options$bayesFactorMatrix <- TRUE
options$model <- "postnumb~prenumb.1 = postnumb~prenumb.2 & postnumb~peabody.1 = postnumb~peabody.2;postnumb~prenumb.1 < postnumb~prenumb.2 & postnumb~peabody.1 < postnumb~peabody.2"
set.seed(1)
results <- jaspTools::runAnalysis("BainSemBayesian", "sesame.csv", options)


test_that("Bayes Factor Matrix table results match", {
	skip_on_os("linux")
	skip_on_os("windows")
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 83.8870391277601, "H1", 0.011920792656384, 1, "H2"))
})

test_that("Coefficients for Parameters table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(115, 0.381341177023541, 0.531813744965293, 0.076773129061891,
			 0.682286312907045, "postnumb~prenumb.1", 115, 0.0651184116547295,
			 0.230979720646418, 0.0846246718306975, 0.396841029638106, "postnumb~peabody.1",
			 115, 0.181822705410796, 0.611842507581534, 0.219401889811588,
			 1.04186230975227, "postnumb~1.1", 125, 0.515320235478543, 0.637881043527746,
			 0.0625321735582629, 0.760441851576949, "postnumb~prenumb.2",
			 125, -0.0907111288570204, 0.0629819901353651, 0.0784162975466372,
			 0.216675109127751, "postnumb~peabody.2", 125, 0.474400149623446,
			 0.966070065783862, 0.250856607590061, 1.45773998194428, "postnumb~1.2"
			))
})

test_that("Bain Structural Equation Model table results match", {
	skip_on_os("linux")
	skip_on_os("windows")
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(13.7330955509952, 13.7330955509952, 0.988219638589409, 0.92188194786564,
			 0.915159866445061, "H1", 0.147562929075261, 0.163709384593724,
			 0.0117803614105911, 0.0109895635541697, 0.0109094310153357,
			 "H2", "", "", "", 0.0671284885801899, "", "Hu", "", 1.10942081198611,
			 "", "", 0.0739307025396031, "Hc"))
})

test_that("Hypothesis Legend table results match", {
	table <- results[["results"]][["legendTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("postnumb~prenumb.1 = postnumb~prenumb.2 &amp; postnumb~peabody.1 = postnumb~peabody.2",
			 "H1", "postnumb~prenumb.1 &lt; postnumb~prenumb.2 &amp; postnumb~peabody.1 &lt; postnumb~peabody.2",
			 "H2"))
})

# ==================================================================================================

# library(bain)
# library(lavaan)
# data("sesamesim")
# model3 <- '
#     postnumb ~ prenumb + peabody 
# '
# sesamesim$sex <- factor(sesamesim$sex, labels = c("boy", "girl"))
# fit3 <- sem(model3, data = sesamesim, std.lv = TRUE, group = "sex")
# hypotheses3 <-
#   "postnumb~prenumb.boy = postnumb~prenumb.girl & postnumb~peabody.boy = postnumb~peabody.girl;
#    postnumb~prenumb.boy < postnumb~prenumb.girl & postnumb~peabody.boy < postnumb~peabody.girl"
# set.seed(100)
# bainResult <- bain(fit3, hypotheses3, fraction = 3, standardize = FALSE)

options <- jaspTools::analysisOptions("BainSemBayesian")
options$syntax <- list("", model = "postnumb ~ prenumb + peabody")
options$fixedFactors <- "sex"
options$descriptives <- TRUE
options$fraction <- 3
options$bayesFactorMatrix <- TRUE
options$model <- "postnumb~prenumb.1 = postnumb~prenumb.2 & postnumb~peabody.1 = postnumb~peabody.2;postnumb~prenumb.1 < postnumb~prenumb.2 & postnumb~peabody.1 < postnumb~peabody.2"
set.seed(1)
results <- jaspTools::runAnalysis("BainSemBayesian", "sesame.csv", options)


test_that("Bayes Factor Matrix table results match", {
	skip_on_os("linux")
	skip_on_os("windows")
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 109.118677436861, "H1", 0.00916433394804139, 1, "H2"))
})

test_that("Coefficients for Parameters table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(115, 0.449638568895951, 0.659515899049615, 0.107082238147818,
			 0.869393229203279, "postnumb~prenumb.1", 115, 0.0461151384254038,
			 0.172521785005472, 0.0644943721298695, 0.29892843158554, "postnumb~peabody.1",
			 115, 3.0054020427868, 7.95455579074895, 2.52512484259938, 12.9037095387111,
			 "postnumb~1.1", 125, 0.548662135224775, 0.723232023628805, 0.0890679062375714,
			 0.897801912032835, "postnumb~prenumb.2", 125, -0.0752470165918699,
			 0.0520870184823556, 0.0649675382193858, 0.179421053556581, "postnumb~peabody.2",
			 125, 6.49955077257414, 11.7022657243836, 2.6544951809563, 16.9049806761931,
			 "postnumb~1.2"))
})

test_that("Bain Structural Equation Model table results match", {
	skip_on_os("linux")
	skip_on_os("windows")
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(15.521998977601, 15.521998977601, 0.99091888839136, 0.931455124849424,
			 0.922652041549263, "H1", 0.12273462966041, 0.142248782171893,
			 0.00908111160864037, 0.00853616582173471, 0.0084554914265996,
			 "H2", "", "", "", 0.0600087093288408, "", "Hu", "", 1.15899467465275,
			 "", "", 0.068892467024138, "Hc"))
})

test_that("Hypothesis Legend table results match", {
	table <- results[["results"]][["legendTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("postnumb~prenumb.1 = postnumb~prenumb.2 &amp; postnumb~peabody.1 = postnumb~peabody.2",
			 "H1", "postnumb~prenumb.1 &lt; postnumb~prenumb.2 &amp; postnumb~peabody.1 &lt; postnumb~peabody.2",
			 "H2"))
})

# ==================================================================================================