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

test_that("incorrect inputs", {

  empty_matrix <- matrix(, nrow = 0, ncol = 1)
  empty_list <- list()

  # empty datasets for methylation and gene data
  expect_error(result <- methylMutationAssociation(
    methylationCancer = empty_matrix,
    methylationNormal = empty_matrix,
    geneCancer = empty_matrix,
    mutationData = empty_list))

  library(MethylMix)
  data("METcancer")
  data("METnormal")
  data("GEcancer")

  # empty list of mutation data
  expect_error(result <- methylMutationAssociation(
    methylationCancer = METcancer,
    methylationNormal = METnormal,
    geneCancer = GEcancer,
    mutationData = empty_list))
})

# [END]
