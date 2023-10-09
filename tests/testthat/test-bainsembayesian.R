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
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, "H1"))
})

test_that("Coefficients for Parameters table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(240, 3.27234343764435, 3.88479958570525, 0.312483368516908, 4.49725573376614,
			 "A=~Ab", 240, 8.33433831455942, 9.58335890609077, 0.637267113775285,
			 10.8323794976221, "A=~Al", 240, 2.94916536411092, 3.36743845907787,
			 0.21340856172166, 3.78571155404482, "A=~Af", 240, 9.71650568863459,
			 10.9063769996814, 0.607088354904675, 12.0962483107283, "A=~An",
			 240, 1.50072596736168, 1.78966463397027, 0.147420395929568,
			 2.07860330057886, "A=~Ar", 240, 3.91023942823756, 4.42382724638746,
			 0.262039416132655, 4.93741506453737, "A=~Ac", 240, 3.80014730487737,
			 4.43337130535907, 0.323079406293428, 5.06659530584077, "B=~Bb",
			 240, 4.14813162554953, 5.05396312117863, 0.462167418776154,
			 5.95979461680774, "B=~Bl", 240, 2.77927605023721, 3.19986341663871,
			 0.214589334150548, 3.62045078304022, "B=~Bf", 240, 7.61169331094648,
			 8.58805986628854, 0.498155355426691, 9.56442642163061, "B=~Bn",
			 240, 1.68630422453705, 1.99705693902588, 0.158550216708065,
			 2.30780965351471, "B=~Br", 240, 3.21686768601433, 3.68521736226071,
			 0.238958307367204, 4.15356703850708, "B=~Bc"))
})

test_that("Bain Structural Equation Model table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(47500350917261.8, 25.870289708301, 1, 0.962784174980776, 0.999999999999979,
			 "H1", "", "", "", 0.0372158250192245, "", "Hu", "", 5.44633654462113e-13,
			 "", "", 2.10524760488996e-14, "Hc"))
})

test_that("Path Diagram plot matches", {
	plotName <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_pathDiagram"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "path-diagram")
})

test_that("Posterior Probabilities plot matches", {
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
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, "H1"))
})

test_that("Coefficients for Parameters table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(240, 0.64333395195267, 0.710450838060892, 0.0342439384792943,
			 0.777567724169114, "A=~Ab", 240, 0.763294389121051, 0.811402927163144,
			 0.0245456235020474, 0.859511465205236, "A=~Al", 240, 0.794122580016282,
			 0.837093624928895, 0.0219244053725287, 0.880064669841508, "A=~Af",
			 240, 0.87697273671862, 0.906133263733766, 0.0148780932941424,
			 0.935293790748912, "A=~An", 240, 0.628742929808782, 0.698041432162551,
			 0.0353570284456176, 0.76733993451632, "A=~Ar", 240, 0.837424918998691,
			 0.873117007119151, 0.0182105836647997, 0.908809095239612, "A=~Ac",
			 240, 0.70761880452078, 0.765839653093138, 0.0297050604151892,
			 0.824060501665495, "B=~Bb", 240, 0.568799849404187, 0.647669783233517,
			 0.0402405015864809, 0.726539717062846, "B=~Bl", 240, 0.760416045819062,
			 0.810184863734924, 0.0253927206359054, 0.859953681650785, "B=~Bf",
			 240, 0.853115564416809, 0.887898658040638, 0.0177468024403481,
			 0.922681751664468, "B=~Bn", 240, 0.654077822367593, 0.720567689980902,
			 0.0339240252054494, 0.787057557594212, "B=~Br", 240, 0.781930329495031,
			 0.828200406721732, 0.0236076160540057, 0.874470483948433, "B=~Bc"
			))
})

test_that("Bain Structural Equation Model table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(717.774530597766, 89.1149117128275, 1, 0.988903057429754, 0.998608743135113,
			 "H1", "", "", "", 0.0110969425702456, "", "Hu", "", 0.12415446343382,
			 "", "", 0.0013912568648869, "Hc"))
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
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 2.76108916214548, 8.08262849360333, "H1", 0.362175917282932,
			 1, 2.92733338872795, "H2", 0.123722128363491, 0.341607827742006,
			 1, "H3"))
})

test_that("Coefficients for Parameters table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(240, 0.644241711285877, 0.711205262171606, 0.0341657047853577,
			 0.778168813057335, "A=~Ab", 240, 0.763763717638553, 0.811775527594915,
			 0.0244962715310449, 0.859787337551277, "A=~Al", 240, 0.79496008776583,
			 0.837770094339868, 0.0218422414451072, 0.880580100913906, "A=~Af",
			 240, 0.877193065218932, 0.906287870649154, 0.0148445612571037,
			 0.935382676079376, "A=~An", 240, 0.629537994692769, 0.69870197505969,
			 0.0352883935176754, 0.76786595542661, "A=~Ar", 240, 0.837895312747232,
			 0.873484275245373, 0.0181579675845388, 0.909073237743515, "A=~Ac",
			 240, 0.707718711787465, 0.765919627657576, 0.0296948904822706,
			 0.824120543527688, "B=~Bb", 240, 0.568915624375763, 0.647765958992103,
			 0.0402305018042685, 0.726616293608442, "B=~Bl", 240, 0.760495386365103,
			 0.810246502727717, 0.02538368906523, 0.859997619090331, "B=~Bf",
			 240, 0.853011516899862, 0.88780309732995, 0.017751132523107,
			 0.922594677760037, "B=~Bn", 240, 0.653863673320625, 0.720382072541199,
			 0.0339385824154234, 0.786900471761773, "B=~Br", 240, 0.782027722082918,
			 0.828276665034914, 0.023596833062649, 0.87452560798691, "B=~Bc",
			 240, 0.729997142131881, 0.788751775228554, 0.029977404462593,
			 0.847506408325226, "A~B", 240, -0.0926531058197054, -0.000478606046975506,
			 0.0470286701693453, 0.0916958937257544, "A~age", 240, -0.10769273254603,
			 -0.0155271511198026, 0.047024119908946, 0.0766384303064246,
			 "A~peabody"))
})

test_that("Bain Structural Equation Model table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(50.2904553874153, 50.2904553874153, 0.672993684142684, 0.664106520751133,
			 0.664620495115328, "H1", 18.2139918105135, 18.2139918105135,
			 0.243742104879996, 0.240523388326618, 0.24070953746343, "H2",
			 6.60908298431283, 6.22204217690021, 0.0832642109773197, 0.0821646722074029,
			 0.0822282622096654, "H3", "", "", "", 0.0132054187148466, "",
			 "Hu", "", 0.941438046952763, "", "", 0.0124417052115763, "Hc"
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
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 6.4492466754354, 37.2604341481214, "H1", 0.155056869480417,
			 1, 5.77748627448894, "H2", 0.0268381199216493, 0.173085655679633,
			 1, "H3"))
})

test_that("Coefficients for Parameters table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(240, 1.97776463189764, 2.39194578953625, 0.211320800231846, 2.80612694717485,
			 "A=~Ab", 240, 5.02018110926225, 5.89871306151199, 0.448238824376105,
			 6.77724501376173, "A=~Al", 240, 1.77583990662079, 2.07359460158627,
			 0.15191845223388, 2.37134929655174, "A=~Af", 240, 5.83194029212427,
			 6.71305697054837, 0.449557586452726, 7.59417364897246, "A=~An",
			 240, 0.907164198528249, 1.1017689666239, 0.0992899714641013,
			 1.29637373471954, "A=~Ar", 240, 2.35075889927942, 2.72333757649134,
			 0.190094654876709, 3.09591625370326, "A=~Ac", 240, 3.80065355470598,
			 4.43383291130091, 0.323056628381624, 5.06701226789583, "B=~Bb",
			 240, 4.14894152081647, 5.05471337859965, 0.462136990744625,
			 5.96048523638284, "B=~Bl", 240, 2.77954352142843, 3.20010329873907,
			 0.214575257825122, 3.6206630760497, "B=~Bf", 240, 7.61072801303948,
			 8.58713247359074, 0.498174695174508, 9.563536934142, "B=~Bn",
			 240, 1.68576714827575, 1.99654229693431, 0.158561662923354,
			 2.30731744559286, "B=~Br", 240, 3.21724343103262, 3.6855573294566,
			 0.238940053040763, 4.15387122788059, "B=~Bc", 240, 1.03048850664578,
			 1.28354041258156, 0.12911048770887, 1.53659231851735, "A~B",
			 240, -0.0240141524531994, -0.00012404673537286, 0.0121890534245877,
			 0.0237660589824537, "A~age", 240, -0.010927740449043, -0.00157515580214843,
			 0.00477181454387254, 0.00777742884474616, "A~peabody"))
})

test_that("Bain Structural Equation Model table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(50.2923775087579, 50.2923775087579, 0.846098857315497, 0.832099940507222,
			 0.83223446446796, "H1", 7.79817861523533, 7.79817861523533,
			 0.131193440086299, 0.129022811869891, 0.129043670734113, "H2",
			 1.3630696215068, 1.3497528587249, 0.0227077025982038, 0.0223319979901301,
			 0.0223356083603207, "H3", "", "", "", 0.0165452496327564, "",
			 "Hu", "", 0.99023031357182, "", "", 0.0163862564376058, "Hc"
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
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 84.7494678913189, "H1", 0.0117994841133679, 1, "H2"))
})

test_that("Coefficients for Parameters table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(115, 0.381341176620643, 0.531813744650272, 0.0767731291067271,
			 0.682286312679901, "postnumb~prenumb.1", 115, 0.0651184115362507,
			 0.230979720241261, 0.0846246716844304, 0.396841028946271, "postnumb~peabody.1",
			 115, 0.181822711674788, 0.61184250948288, 0.219401887585708,
			 1.04186230729097, "postnumb~1.1", 125, 0.515320235060605, 0.637881043952246,
			 0.0625321739880859, 0.760441852843887, "postnumb~prenumb.2",
			 125, -0.0907111262668841, 0.0629819922557709, 0.0784162973069744,
			 0.216675110778426, "postnumb~peabody.2", 125, 0.47440014333607,
			 0.966070060702074, 0.250856608205168, 1.45773997806808, "postnumb~1.2"
			))
})

test_that("Bain Structural Equation Model table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(13.7330959747582, 13.7330959747582, 0.98833812005379, 0.921985057352127,
			 0.916102818607097, "H1", 0.147898338831813, 0.162043447781516,
			 0.0116618799462106, 0.010878948036989, 0.010809540654366, "H2",
			 "", "", "", 0.0671359946108844, "", "Hu", "", 1.09564075608577,
			 "", "", 0.0730876407385372, "Hc"))
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
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_bayesFactorMatrix"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 112.540307029178, "H1", 0.00888570527660576, 1, "H2"))
})

test_that("Coefficients for Parameters table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_descriptivesTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(115, 0.449638568174712, 0.659515898444714, 0.107082238207175,
			 0.869393228714715, "postnumb~prenumb.1", 115, 0.0461151379966759,
			 0.172521784646813, 0.0644943721656198, 0.298928431296951, "postnumb~peabody.1",
			 115, 3.00540206217884, 7.95455581288439, 2.5251248439991, 12.9037095635899,
			 "postnumb~1.1", 125, 0.54866213529346, 0.723232023081061, 0.0890679059230611,
			 0.897801910868663, "postnumb~prenumb.2", 125, -0.0752470144627417,
			 0.0520870201618504, 0.0649675379899767, 0.179421054786442, "postnumb~peabody.2",
			 125, 6.49955071273804, 11.7022656461761, 2.65449517158296, 16.9049805796142,
			 "postnumb~1.2"))
})

test_that("Bain Structural Equation Model table results match", {
	table <- results[["results"]][["bainContainer"]][["collection"]][["bainContainer_mainResultsTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(15.5219994860316, 15.5219994860316, 0.991192555083165, 0.931696930081885,
			 0.923294273373419, "H1", 0.119765379181874, 0.137923912736503,
			 0.00880744491683483, 0.00827878432782599, 0.00820412079677407,
			 "H2", "", "", "", 0.0600242855902893, "", "Hu", "", 1.15161755157184,
			 "", "", 0.0685016058298068, "Hc"))
})

test_that("Hypothesis Legend table results match", {
	table <- results[["results"]][["legendTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("postnumb~prenumb.1 = postnumb~prenumb.2 &amp; postnumb~peabody.1 = postnumb~peabody.2",
			 "H1", "postnumb~prenumb.1 &lt; postnumb~prenumb.2 &amp; postnumb~peabody.1 &lt; postnumb~peabody.2",
			 "H2"))
})

# ==================================================================================================