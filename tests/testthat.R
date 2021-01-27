library(jaspTools)
library(testthat)

install.packages("bain") # This needs to be removed when bain 0.2.4 is in the required-files

jaspTools::runTestsTravis(module = getwd())
