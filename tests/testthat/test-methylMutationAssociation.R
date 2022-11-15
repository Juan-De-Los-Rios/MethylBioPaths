library("MethylBioPaths")

test_that("function generates the correct data and plot", {

  library(MethylMix)
  data("METcancer")
  data("METnormal")
  data("GEcancer")
  data("mutationExample")

  result <-
    methylMutationAssociation(METcancer, METnormal, GEcancer, mutationExample)

  expect_type(result, "NULL")
})

# [END]
