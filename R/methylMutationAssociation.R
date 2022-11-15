#' Methylation-Mutation Association
#'
#' A function that generates a plot highlighting the relationship between
#' mutation category and differential DNA methylation.
#'
#' @param methylationCancer Matrix containing the methylation data
#' belonging to a sample representative of disease being studied
#' @param methylationNormal Matrix containing the methylation data
#' belonging to a sample representative of baseline methylation
#' @param geneCancer Matrix containing the gene expression data belonging to
#' a sample representative of the disease being studied
#' @param mutationData List organizing genes based on the category of their
#' mutation
#'
#' @examples
#' library(MethylMix)
#' data(METcancer)
#' data(METnormal)
#' data(GEcancer)
#' data(mutationExample)
#' methylMutationAssociation(METcancer, METnormal, GEcancer, mutationExample)
#'
#'
#' @references
#' Gevaert O (2022). MethylMix: Identifying methylation driven
#' cancer genes. R package version 2.26.0.
#'
#' R Core Team (2022). R: A language and environment for statistical computing.
#' R Foundation for Statistical Computing, Vienna, Austria.
#' URL https://www.R-project.org/
#'
#' @export
#' @import MethylMix
#' @import graphics
methylMutationAssociation <- function(methylationCancer,
                                      methylationNormal,
                                      geneCancer,
                                      mutationData) {

  if (is.null(names(mutationData)) ||
      length(mutationData[is.na(names(mutationData))])) {
    stop("List containing mutation data must name each entry based on the
         mutation type it represents")
  }

  if (dim(geneCancer)[1] * dim(geneCancer)[2] == 0 ||
      dim(methylationCancer)[1] * dim(methylationCancer)[2] == 0 ||
      dim(methylationNormal)[1] * dim(methylationNormal)[2] == 0) {
    stop("Inputs containing methylation and gene samples must not be empty")
  }

  # Determining driver genes for methylation in gene sample
  methylMixRes <- MethylMix::MethylMix(METcancer = methylationCancer,
                       METnormal = methylationNormal,
                       GEcancer = geneCancer)

  assocList <- list()

  # Filling assocList with DM-values corresponding to the category of their
  # driver gene specified in mutationData
  for (i in seq_along(mutationData)) {
    assocList[[names(mutationData)[i]]] <-
      as.vector(unlist(methylMixRes$MixtureStates[mutationData[[i]]]))
  }

  boxplot(assocList,
          main = "Association between differential methylation and mutation category",
          xlab = "Mutation category",
          ylab = "Differential methylation value")

  # Boxplot does not display a graphic if returned as an object, so function
  # returns nothing
  return(invisible(NULL))
}
# [END]
