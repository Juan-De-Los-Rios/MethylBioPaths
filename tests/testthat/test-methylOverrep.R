library("MethylBioPaths")

test_that("function generates the correct data and plot", {

  library(MethylMix)
  data("METcancer")
  data("METnormal")
  data("GEcancer")

  plot <- methylOverrep(METcancer, METnormal, GEcancer, 1, 1)

  expect_type(plot, "list")
})

test_that("incorrect inputs", {

  empty_matrix <- matrix(, nrow = 0, ncol = 1)
  empty_list <- list()

  expect_error(plot <- methylOverrep(methylationCancer = empty_matrix,
                        methylationNormal = empty_matrix,
                        geneCancer = empty_matrix,
                        pValue = 0.05,
                        qValue = 0.1))

  library(MethylMix)
  data("METcancer")
  data("METnormal")
  data("GEcancer")

  expect_error(plot <- methylOverrep(methylationCancer = METcancer,
                                     methylationNormal = METnormal,
                                     geneCancer = GEcancer,
                                     pValue = 2,
                                     qValue = 2))
})

# [END]
