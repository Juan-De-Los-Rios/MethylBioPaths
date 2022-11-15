library("MethylBioPaths")

test_that("function generates the correct data and plot", {

  library(MethylMix)
  data("METcancer")
  data("METnormal")
  data("GEcancer")

  plot <- methylOverrep(METcancer, METnormal, GEcancer, 1, 1)

  expect_type(plot, "list")
})

# [END]
